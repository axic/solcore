module Solcore.Frontend.ComptimeCheck (checkComptimeEarly) where

{- SAIL-level comptime verification pass.
   Runs on CompUnit Id immediately after type checking.

   Classification uses three states:
     CTComptime  — definitely comptime: literal, comptime-bound variable, or
                   a call to a function annotated '-> comptime' with all
                   comptime-param arguments classified as CTComptime.
     CTRuntime   — definitely not comptime: a variable bound by a non-comptime
                   function parameter.
     CTDeferred  — uncertain: call results where the function has no comptime
                   return annotation, or unannotated let bindings.
                   These are passed to the MAST-level verifier.

   Errors are reported only for CTRuntime violations:
     - A parameter annotated 'comptime' receives a CTRuntime argument.
     - A 'let x : comptime T = e' binding where e classifies as CTRuntime.

   CTDeferred values are never rejected here; the MAST-level pass handles them.

   Separately, an assembly block may not assign (via Yul ':=') to a variable
   that is erased before Hull emission — an explicitly 'comptime' parameter or
   let, or a comptime-only-typed (string / integer) one.  Such a variable has no
   runtime representation, so an asm assignment to it would leave a dangling
   identifier in the generated Yul.  This must be caught here, on the typed AST,
   because the declaration is gone by the time the MAST-level pass runs.
-}

import Data.Map qualified as Map
import Data.Set qualified as Set
import Language.Yul (YulBlock, YulStmt (..))
import Solcore.Frontend.Syntax.Contract
import Solcore.Frontend.Syntax.Name (Name)
import Solcore.Frontend.Syntax.Stmt
import Solcore.Frontend.Syntax.Ty
import Solcore.Frontend.TypeInference.Id (Id (..))
import Solcore.Primitives.Primitives qualified as Prim

-----------------------------------------------------------------------
-- Comptime-ness classification
-----------------------------------------------------------------------

data Ctness = CTComptime | CTRuntime | CTDeferred
  deriving (Eq, Show)

-----------------------------------------------------------------------
-- Signature table: Name -> Signature Id
-----------------------------------------------------------------------

type SigTable = Map.Map Name (Signature Id)

buildSigTable :: CompUnit Id -> SigTable
buildSigTable (CompUnit _ topDecls) = Map.fromList $ concatMap fromTopDecl topDecls
  where
    fromTopDecl (TFunDef fd) = [(sigName (funSignature fd), funSignature fd)]
    fromTopDecl (TContr c) = concatMap fromContrDecl (decls c)
    fromTopDecl (TClassDef cl) = [(sigName s, s) | s <- signatures cl]
    fromTopDecl (TInstDef inst) = [(sigName (funSignature fd), funSignature fd) | fd <- instFunctions inst]
    fromTopDecl _ = []

    fromContrDecl (CFunDecl fd) = [(sigName (funSignature fd), funSignature fd)]
    fromContrDecl _ = []

-----------------------------------------------------------------------
-- Comptime environment: variable name -> Ctness
-----------------------------------------------------------------------

type CtEnv = Map.Map Name Ctness

-----------------------------------------------------------------------
-- Entry point
-----------------------------------------------------------------------

-- | Run the early comptime check on a typed compilation unit.
checkComptimeEarly :: CompUnit Id -> Either String ()
checkComptimeEarly cu = mapM_ (checkTopDecl st) (contracts cu)
  where
    st = buildSigTable cu

checkTopDecl :: SigTable -> TopDecl Id -> Either String ()
checkTopDecl st (TFunDef fd) = checkFunDef st ctx fd
  where
    ctx = "function '" ++ show (sigName (funSignature fd)) ++ "'"
checkTopDecl st (TContr c) = mapM_ (checkContrDecl st) (decls c)
checkTopDecl st (TInstDef inst) = mapM_ (checkFunDefInst st inst) (instFunctions inst)
checkTopDecl _ _ = Right ()

checkContrDecl :: SigTable -> ContractDecl Id -> Either String ()
checkContrDecl st (CFunDecl fd) = checkFunDef st ctx fd
  where
    ctx = "function '" ++ show (sigName (funSignature fd)) ++ "'"
checkContrDecl _ _ = Right ()

-----------------------------------------------------------------------
-- Function checking
-----------------------------------------------------------------------

checkFunDef :: SigTable -> String -> FunDef Id -> Either String ()
checkFunDef st ctx fd = do
  checkBody st (effRetComptime sig) ctx initEnv (funDefBody fd)
  checkNoAsmComptimeAssign ctx (erasableNames sig (funDefBody fd)) (funDefBody fd)
  where
    sig = funSignature fd
    -- For '-> comptime' functions, treat ALL params as CTComptime when checking
    -- the body: this verifies "given comptime args, does the body produce comptime?"
    -- A param of comptime-only type (string/integer) is also implicitly comptime,
    -- since such values exist only at compile time.  Other params are CTRuntime.
    initEnv =
      Map.fromList
        [ (idName (paramName p), if paramComptime p || effRetComptime sig || isComptimeOnlyTy (paramTy p) then CTComptime else CTRuntime)
        | p <- sigParams sig
        ]

-- | Check an instance method, including the instance head in error context.
checkFunDefInst :: SigTable -> Instance Id -> FunDef Id -> Either String ()
checkFunDefInst st inst fd = checkFunDef st ctx fd
  where
    ctx =
      "in instance "
        ++ tyHeadName (mainTy inst)
        ++ ":"
        ++ show (instName inst)
        ++ ", function '"
        ++ show (sigName (funSignature fd))
        ++ "'"

-- | Extract a readable name from a concrete type (e.g. @word@ from @TyCon "word" []@).
tyHeadName :: Ty -> String
tyHeadName (TyCon n _) = show n
tyHeadName t = show t

checkBody :: SigTable -> Bool -> String -> CtEnv -> Body Id -> Either String ()
checkBody _ _ _ _ [] = Right ()
checkBody st retCt ctx env (s : ss) = do
  env' <- checkStmt st retCt ctx env s
  checkBody st retCt ctx env' ss

checkStmt :: SigTable -> Bool -> String -> CtEnv -> Stmt Id -> Either String CtEnv
checkStmt st retCt ctx env stmt = case stmt of
  Let ct x _ mInit -> do
    case mInit of
      Nothing -> return env
      Just e -> do
        checkExp st env e
        let ct' = classifyExp st env e
        when_ (ct && ct' == CTRuntime) $
          "comptime let '"
            ++ show (idName x)
            ++ "' is bound to a runtime expression"
        return $ Map.insert (idName x) (letCtness ct ct') env
  (_ := e) -> checkExp st env e >> return env
  StmtExp e -> checkExp st env e >> return env
  Return e -> do
    checkExp st env e
    when_ (retCt && classifyExp st env e == CTRuntime) $
      ctx ++ ": function annotated '-> comptime' returns a runtime expression"
    return env
  Match es eqs -> do
    mapM_ (checkExp st env) es
    mapM_ (checkEq st retCt ctx env) eqs
    return env
  If cond t f -> do
    checkExp st env cond
    checkBody st retCt ctx env t
    checkBody st retCt ctx env f
    return env
  For initStmt _ postStmt body -> do
    _ <- checkStmt st retCt ctx env initStmt
    checkBody st retCt ctx env body
    _ <- checkStmt st retCt ctx env postStmt
    return env
  Asm _ -> return env
  Block body -> checkBody st retCt ctx env body >> return env
  Break -> return env
  Continue -> return env
  EmptyStmt -> return env

-- | Decide the Ctness to assign to a let-bound variable.
--   If declared comptime, treat as CTComptime (Stage 1 verifies the RHS).
--   Otherwise, inherit the classification of the init expression.
letCtness :: Bool -> Ctness -> Ctness
letCtness True _ = CTComptime
letCtness False ct' = ct'

checkEq :: SigTable -> Bool -> String -> CtEnv -> ([Pat Id], Body Id) -> Either String ()
checkEq st retCt ctx env (_, body) = checkBody st retCt ctx env body

-----------------------------------------------------------------------
-- Expression checking: recurse and enforce comptime-param constraints
-----------------------------------------------------------------------

checkExp :: SigTable -> CtEnv -> Exp Id -> Either String ()
checkExp st env (Call _ f args) = do
  checkCallSite st env f args
  mapM_ (checkExp st env) args
checkExp st env (Con _ args) = mapM_ (checkExp st env) args
checkExp st env (Cond c t e) = mapM_ (checkExp st env) [c, t, e]
checkExp st env (TyExp e _) = checkExp st env e
checkExp st env (Lam ps body _) = checkBody st False "lambda" lamEnv body
  where
    lamEnv =
      Map.fromList
        [(idName (paramName p), if paramComptime p then CTComptime else CTRuntime) | p <- ps]
        `Map.union` env
checkExp _ _ _ = Right ()

-- | Verify that each comptime-annotated parameter receives a non-Runtime arg.
--   Skips polymorphic signatures (those whose comptime parameter types contain
--   type variables): the concrete types are only known after specialisation,
--   so polymorphic calls are deferred to the MAST-level check.
checkCallSite :: SigTable -> CtEnv -> Id -> [Exp Id] -> Either String ()
checkCallSite st env f args =
  case Map.lookup (idName f) st of
    Nothing -> Right ()
    Just sig
      | any (hasTypeVar . paramTy) (filter paramComptime (sigParams sig)) ->
          Right () -- polymorphic comptime param — defer to MAST-level check
      | otherwise ->
          mapM_ checkArg (zip (sigParams sig) args)
  where
    checkArg (param, arg) =
      when_ (paramComptime param && classifyExp st env arg == CTRuntime) $
        "runtime value passed to comptime parameter '"
          ++ show (idName (paramName param))
          ++ "' of '"
          ++ show (idName f)
          ++ "'"

-- | A value of comptime-only type (string / integer) exists only at compile
-- time, so it is always comptime regardless of annotation.
isComptimeOnlyTy :: Ty -> Bool
isComptimeOnlyTy t = t == Prim.string || t == Prim.integer

-- | A function is effectively '-> comptime' if it is annotated so, or if its
-- return type is comptime-only.
effRetComptime :: Signature Id -> Bool
effRetComptime sig = sigRetComptime sig || maybe False isComptimeOnlyTy (sigReturn sig)

paramTy :: Param Id -> Ty
paramTy (Typed _ _ ty) = ty
paramTy (Untyped _ _) = TyCon (error "paramTy: Untyped") []

-- | True if the type contains any type variable or meta variable.
hasTypeVar :: Ty -> Bool
hasTypeVar (TyVar _) = True
hasTypeVar (Meta _) = True
hasTypeVar (TyCon _ ts) = any hasTypeVar ts

-----------------------------------------------------------------------
-- Expression classification
-----------------------------------------------------------------------

classifyExp :: SigTable -> CtEnv -> Exp Id -> Ctness
classifyExp _ _ (Lit _) = CTComptime
classifyExp _ env (Var x) = Map.findWithDefault CTDeferred (idName x) env
classifyExp st env (TyExp e _) = classifyExp st env e
classifyExp st env (Call _ f args) = classifyCall st env f args
classifyExp st env (Con _ args) = combineCt (map (classifyExp st env) args)
classifyExp st env (Cond c t e) = combineCt (map (classifyExp st env) [c, t, e])
classifyExp _ _ _ = CTDeferred

-- | Combine a list of Ctness values: all Comptime → Comptime;
--   any Runtime → Runtime; otherwise Deferred.
combineCt :: [Ctness] -> Ctness
combineCt cts
  | all (== CTComptime) cts = CTComptime
  | any (== CTRuntime) cts = CTRuntime
  | otherwise = CTDeferred

-- | Classify a function call result.
--   CTComptime iff the function is annotated '-> comptime' and ALL arguments
--   are CTComptime.  A non-comptime-annotated param in a '-> comptime' function
--   means "result is comptime when this arg happens to be comptime", so all args
--   must be checked, not just the comptime-annotated ones.
--   Never CTRuntime for calls — uncertain cases are deferred to MAST.
classifyCall :: SigTable -> CtEnv -> Id -> [Exp Id] -> Ctness
classifyCall st env f args =
  case Map.lookup (idName f) st of
    Nothing -> CTDeferred
    Just sig
      | effRetComptime sig && allArgsComptime ->
          CTComptime
      | otherwise ->
          CTDeferred
  where
    allArgsComptime = all (\arg -> classifyExp st env arg == CTComptime) args

-----------------------------------------------------------------------
-- Helper
-----------------------------------------------------------------------

when_ :: Bool -> String -> Either String ()
when_ True msg = Left msg
when_ False _ = Right ()

-----------------------------------------------------------------------
-- Assembly may not assign to an erased (comptime) variable
-----------------------------------------------------------------------

-- | Names of variables in a function that are erased before Hull emission:
--   explicitly-comptime or comptime-only-typed parameters, and likewise-declared
--   lets anywhere in the body.  Assigning any of these inside an asm block leaves
--   a dangling Yul identifier.
erasableNames :: Signature Id -> Body Id -> Set.Set Name
erasableNames sig body =
  Set.fromList [idName (paramName p) | p <- sigParams sig, paramErasable p]
    `Set.union` bodyErasableLets body

paramErasable :: Param Id -> Bool
paramErasable (Typed ct _ ty) = ct || isComptimeOnlyTy ty
paramErasable (Untyped ct _) = ct

bodyErasableLets :: Body Id -> Set.Set Name
bodyErasableLets = foldMap stmtErasableLets

stmtErasableLets :: Stmt Id -> Set.Set Name
stmtErasableLets (Let ct x mty _)
  | ct || maybe False isComptimeOnlyTy mty = Set.singleton (idName x)
  | otherwise = Set.empty
stmtErasableLets (Match _ eqs) = foldMap (bodyErasableLets . snd) eqs
stmtErasableLets (If _ t f) = bodyErasableLets t `Set.union` bodyErasableLets f
stmtErasableLets (For i _ p b) =
  stmtErasableLets i `Set.union` stmtErasableLets p `Set.union` bodyErasableLets b
stmtErasableLets (Block b) = bodyErasableLets b
stmtErasableLets _ = Set.empty

-- | Reject any asm block that assigns to an erased (comptime) variable.
checkNoAsmComptimeAssign :: String -> Set.Set Name -> Body Id -> Either String ()
checkNoAsmComptimeAssign ctx erasable = mapM_ goStmt
  where
    goStmt (Asm blk) =
      case filter (`Set.member` erasable) (asmExternalAssignTargets blk) of
        (n : _) ->
          Left $
            ctx
              ++ ": assembly block assigns to comptime variable '"
              ++ show n
              ++ "', which has no runtime representation"
        [] -> Right ()
    goStmt (Match _ eqs) = mapM_ (checkNoAsmComptimeAssign ctx erasable . snd) eqs
    goStmt (If _ t f) =
      checkNoAsmComptimeAssign ctx erasable t
        >> checkNoAsmComptimeAssign ctx erasable f
    goStmt (For i _ p b) =
      goStmt i >> goStmt p >> checkNoAsmComptimeAssign ctx erasable b
    goStmt (Block b) = checkNoAsmComptimeAssign ctx erasable b
    goStmt _ = Right ()

-- | Names assigned to (Yul ':=') in an asm block that are NOT Yul-local (declared
--   by a 'let' inside the block).  A block-local shadows an outer variable, so
--   assigning it is not an assignment to the erased outer one.
asmExternalAssignTargets :: YulBlock -> [Name]
asmExternalAssignTargets blk =
  [n | n <- yulAssignTargets blk, not (n `Set.member` locals)]
  where
    locals = Set.fromList (yulLetNames blk)

yulAssignTargets :: YulBlock -> [Name]
yulAssignTargets = concatMap go
  where
    go (YAssign names _) = names
    go (YIf _ b) = yulAssignTargets b
    go (YBlock b) = yulAssignTargets b
    go (YFor pre _ post body) = yulAssignTargets (pre ++ post ++ body)
    go (YSwitch _ cases def) =
      concatMap (yulAssignTargets . snd) cases ++ maybe [] yulAssignTargets def
    go (YFun {}) = [] -- nested Yul function: its own scope
    go _ = []

yulLetNames :: YulBlock -> [Name]
yulLetNames = concatMap go
  where
    go (YLet names _) = names
    go (YIf _ b) = yulLetNames b
    go (YBlock b) = yulLetNames b
    go (YFor pre _ post body) = yulLetNames (pre ++ post ++ body)
    go (YSwitch _ cases def) =
      concatMap (yulLetNames . snd) cases ++ maybe [] yulLetNames def
    go (YFun {}) = []
    go _ = []
