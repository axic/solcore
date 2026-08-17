{-# LANGUAGE OverloadedRecordDot #-}

module Solcore.Desugarer.FieldAccess (fieldDesugarTopDecls, fieldDesugarer) where

import Control.Monad.Reader (MonadReader (..))
-- import Data.Generics(Data, mkT, everywhere)
import Data.List (mapAccumL)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (isJust)
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Stack
import Solcore.Desugarer.StructProjection (isStructDataTy, structFieldMap)
import Solcore.Frontend.Pretty.SolcorePretty
import Solcore.Frontend.Syntax hiding (name)
import Solcore.Frontend.Syntax.Contract qualified as Contract
import Solcore.Primitives.Primitives hiding (arr)
import Prelude hiding (exp)

type ContractName = Name

type NmContract = Contract Name

type NmField = Field Name

type NmTopDecl = TopDecl Name

type NmContractDecl = ContractDecl Name

type NmBody = Body Name

type NmStmt = Stmt Name

type NmExp = Exp Name

type NmEquation = Equation Name

fieldDesugarer :: CompUnit Name -> CompUnit Name
fieldDesugarer (CompUnit ims topdecls) = CompUnit ims (fieldDesugarTopDecls topdecls)

fieldDesugarTopDecls :: [TopDecl Name] -> [TopDecl Name]
fieldDesugarTopDecls topdecls = extras <> structExtras <> topdecls'
  where
    existingDataTypes =
      Set.fromList
        [ dataName dt
        | TDataDef dt <- topdecls
        ]
    -- struct type name -> field names, for recognising struct-typed storage
    -- fields when desugaring a sub-field access (f.x / f.x = v).
    structs = Map.fromList (structFieldMap [dt | TDataDef dt <- topdecls])
    -- Struct DataTys by name, so a contract field's declared type can be
    -- resolved back to its struct definition.
    structByName =
      Map.fromList
        [ (dataName dt, dt)
        | TDataDef dt <- topdecls,
          isStructDataTy dt
        ]
    -- Structs that appear as a contract *storage* field's type. Only these get
    -- the per-field member-access machinery emitted: the generated instances
    -- reference std classes (StructField/CStructField/StorageSize), so a value
    -- struct in a std-less module must not drag them in.
    storageStructNames =
      Set.fromList
        [ structName
        | TContr c <- topdecls,
          f <- getFields (Contract.decls c),
          Just structName <- [tyConNameOf (fieldTy f)],
          structName `Map.member` structByName
        ]
    structExtras =
      concat
        [ extraTopDeclsForStruct dt
        | (nm, dt) <- Map.toList structByName,
          nm `Set.member` storageStructNames
        ]
    (extras, topdecls') = mapAccumL go mempty topdecls
    go acc (TContr c) =
      let hasSingletonCollision =
            singletonNameForContract (Contract.name c) `Set.member` existingDataTypes
       in (acc <> extraTopDeclsForContract (not hasSingletonCollision) c, TContr (transContract structs c))
    go acc v = (acc, v)

--------------------------------
-- # Extra Top Decls
--------------------------------

extraTopDeclsForContract :: Bool -> NmContract -> [NmTopDecl]
extraTopDeclsForContract includeSingleton (Contract cname _ts cdecls) = do
  let singName = singletonNameForContract cname
  let contractSingDecl = TDataDef $ DataTy singName [] [Constr singName [] []] []

  let fields = getFields cdecls
  let (_fieldTypes, extraFieldDecls) = foldl' (flip contractFieldStep) ([], []) fields
  (if includeSingleton then contractSingDecl : extraFieldDecls else extraFieldDecls)
  where
    -- given a list of contract field types so far and topdecls for them, amends them with data for another field
    -- the types of previous fields are needed to construct field offset
    contractFieldStep :: NmField -> ([Ty], [NmTopDecl]) -> ([Ty], [NmTopDecl])
    contractFieldStep field (tys, topdecls) = (tys', topdecls')
      where
        tys' = tys ++ [fieldTy field]
        topdecls' = topdecls ++ extraTopDeclsForContractField cname field offset
        offset = foldr pair unit tys

extraTopDeclsForContractField :: ContractName -> NmField -> Ty -> [NmTopDecl]
extraTopDeclsForContractField cname (Field fname fty _minit) offset = [selDecl, TInstDef sfInstance]
  where
    -- data b_sel = n_sel
    selName = selectorNameForField cname fname
    selDecl = TDataDef $ DataTy selName [] [Constr selName [] []] []
    selType = TyCon selName []
    -- instance StructField(ContractStorage(CCtx), fld1_sel):CStructField(uint, ()) {}
    ctxTy = TyCon "ContractStorage" [singletonTypeForContract cname]
    sfInstance =
      Instance
        { instDefault = False,
          instVars = [],
          instContext = [],
          instName = "CStructField",
          paramsTy = [translateFieldType fty, offset],
          mainTy = TyCon "StructField" [ctxTy, selType],
          instFunctions = []
        }

translateFieldType :: Ty -> Ty
translateFieldType t = TyCon "storage" [t]

-- Per-field member-access machinery for a struct held in storage. For each
-- field we emit a nullary selector data type and a `StructField(S, sel) :
-- CStructField(fieldType, offset)` instance, where `offset` is the pair-encoded
-- list of the *preceding* field types (so `StorageSize(offset)` is the field's
-- slot displacement from the struct base). Together with the std
-- `MemberAccessProxy(storage(S), sel, ...):LVA/RVA` instances this lets `s.x`
-- read/write only that field's slot(s) — no whole-struct load/store.
--
-- Mirrors 'extraTopDeclsForContractField', but the base is the struct's own
-- storage handle (slot 0 is the contract-field case) and the field type is the
-- bare type, not `storage(_)` (the std instance re-wraps it).
extraTopDeclsForStruct :: DataTy -> [NmTopDecl]
extraTopDeclsForStruct dt@(DataTy _ params [Constr _ tys fields] _) =
  concat
    [ [selDecl fld, TInstDef (sfInstance fld fty offset)]
    | i <- [0 .. length fields - 1],
      let fld = fields !! i
          fty = tys !! i
          offset = foldr pair unit (take i tys)
    ]
  where
    structTy = TyCon (dataName dt) (map TyVar params)
    selDecl fld =
      let selName = structFieldSelName (dataName dt) fld
       in TDataDef $ DataTy selName [] [Constr selName [] []] []
    sfInstance fld fty offset =
      Instance
        { instDefault = False,
          instVars = params,
          instContext = [],
          instName = "CStructField",
          paramsTy = [fty, offset],
          mainTy = TyCon "StructField" [structTy, TyCon (structFieldSelName (dataName dt) fld) []],
          instFunctions = []
        }
extraTopDeclsForStruct _ = []

--------------------------------
-- # Contract Desugaring
--------------------------------
-- the desugaring behaves mostly like a Reader, but semetimes we want to make the environment explicit
-- Note that we cannot simply use `everywhere` - this would require at least three passes
-- - desugar assignments
-- - desugar indexing
-- - desugar field accesses
-- lest we inadvertenly mistranslate a LHS as a RHS

data ContractEnv = CEnv
  { ceName :: Name,
    ceFields :: Map Name NmField,
    ceLocals :: Set Name,
    -- struct type name -> field names (in declaration order)
    ceStructs :: Map Name [Name]
  }

type CEM a = ContractEnv -> a

transContract :: Map Name [Name] -> NmContract -> NmContract
transContract structs c = c {decls = concatMap (flip transCDecl cenv) (Contract.decls c)}
  where
    cenv =
      CEnv
        { ceName = Contract.name c,
          ceFields = Map.fromList [(fieldName f, f) | f <- getFields (Contract.decls c)],
          ceLocals = mempty,
          ceStructs = structs
        }

transCDecl :: NmContractDecl -> CEM [NmContractDecl]
transCDecl (CFunDecl fd) = do
  body' <- transBody fd.funDefBody
  pure [CFunDecl fd {funDefBody = body'}]
transCDecl (CConstrDecl cd) = do
  body' <- transBody cd.constrBody
  pure [CConstrDecl cd {constrBody = body'}]
transCDecl CFieldDecl {} = pure []
transCDecl d = pure [d]

transBody :: NmBody -> ContractEnv -> NmBody
transBody body cenv = snd $ mapAccumL transStmt cenv body

transStmt :: ContractEnv -> NmStmt -> (ContractEnv, NmStmt)
transStmt cenv (Let c x mty me) = (cenv {ceLocals = Set.insert x cenv.ceLocals}, Let c x mty me')
  where
    me' = flip transRhs cenv <$> me
transStmt cenv stmt = (cenv, go stmt cenv)
  where
    go :: NmStmt -> CEM NmStmt
    go (lhs := rhs) = transAssignment lhs rhs
    go (Return exp) = Return <$> transRhs exp
    go (Block body) = pure (Block (transBody body cenv))
    go (StmtExp exp) = StmtExp <$> transRhs exp
    go (If e b1 b2) = If <$> transRhs e <*> transBody b1 <*> transBody b2
    go (For initStmt cond postStmt body) =
      pure $ For initStmt' cond' postStmt' body'
      where
        (forEnv, initStmt') = transStmt cenv initStmt
        cond' = transRhs cond forEnv
        (_, postStmt') = transStmt forEnv postStmt
        body' = transBody body forEnv
    go (Match es eqns) = traces [pretty (r cenv)] r where r = Match <$> mapM transRhs es <*> mapM transEquation eqns
    go Let {} = error "Impossible"
    go s@Asm {} = pure s
    go Break = pure Break
    go Continue = pure Continue
    go EmptyStmt = pure EmptyStmt

-- go s = pure s

transEquation :: NmEquation -> CEM NmEquation
transEquation (pats, body) cenv = (pats, transBody body cenv)

transAssignment :: NmExp -> NmExp -> ContractEnv -> NmStmt
transAssignment lhs@(Var x) rhs cenv
  | isLocal x cenv =
      traces
        ["tA: Assignment to local var ", show x]
        (lhs := rhs')
  | Just _fty <- askFieldTy x cenv =
      traces
        ["tA: Assignment to variable which is a field and not local:", show x]
        (lhs := rhs')
  where
    rhs' = transRhs rhs cenv
transAssignment lhs@(FieldAccess Nothing x) rhs cenv
  | isLocal x cenv =
      traces
        ["tA: Assignment to a field which is shadowed by local var:", pretty x]
        (Var x := rhs')
  | Just _fty <- askFieldTy x cenv =
      traces
        ["tA: Assignment a contract field which is not local:", pretty (lhs := rhs)]
        (transContractFieldAssignment x rhs' cenv)
  where
    rhs' = transRhs rhs cenv
transAssignment (Indexed arr idx) rhs cenv = do
  let idx' = traces ["transRhs", pretty idx] $ transRhs idx cenv
  let lhs' = traces ["lhsIndex", pretty arr, pretty idx] $ lhsIndex arr idx' cenv
  let rhs' = traces ["transRhs", pretty rhs] $ transRhs rhs cenv
  let assignName = QualName (Name "Assign") "assign"
  StmtExp $ Call Nothing assignName [lhs', rhs']
-- Sub-field write to a struct-typed contract (storage) field: `f.x = v`.
-- Desugar it to a member-access chain that assigns only field x's slot(s):
--
--   Assign.assign(LVA.acc(MemberAccessProxy(<f as storage(Point)>, Point_x_ssel)), v)
--
-- The whole struct is never loaded or rewritten — only the field's own slot is
-- SSTOREd, unlike the read-modify-write an MVP would use.
transAssignment (FieldAccess (Just (FieldAccess Nothing f)) field) rhs cenv
  | not (isLocal f cenv),
    Just fty <- askFieldTy f cenv,
    Just structName <- tyConNameOf fty,
    Just fieldNames <- Map.lookup structName (ceStructs cenv),
    field `elem` fieldNames =
      let lhs' = lhsAccess (structMemberProxy f structName field cenv)
          rhs' = transRhs rhs cenv
          assignName = QualName (Name "Assign") "assign"
       in StmtExp $ Call Nothing assignName [lhs', rhs']
transAssignment lhs rhs cenv =
  traces ["Other assignment:", pretty (lhs := rhs)] $
    (lhs := rhs')
  where
    rhs' = transRhs rhs cenv

transContractFieldAssignment :: Name -> NmExp -> CEM NmStmt
transContractFieldAssignment field rhs = do
  {- Desugaring scheme:
         // this.counter = rhs
         let cxt = ContractStorage(CounterCxt);
         let counter_map : MemberAccessProxy(cxt, counter_sel, ())
         = MemberAccessProxy(cxt, counter_sel);
         let counter_lval : storageRef(word)
                          = LVA.acc(counter_map);
         let counter_rval : word
                          = RVA.acc(counter_map);
         Assign.assign(counter_lval, counter_rval);
  -}
  fieldMap <- memberProxyFor field
  let lhs' = lhsAccess fieldMap
  rhs' <- transRhs rhs
  -- An array literal on the right is a memory -> storage array copy, which
  -- cannot be an Assign/CanStore instance: instance overlap is decided by the
  -- main type alone, so it would clash with the storage-to-storage deep copy.
  -- Route it to the plain std function instead. Purely syntactic; if the field
  -- is not a storage array, storeArrayLit simply fails to unify.
  let fun = case rhs of
        ArrayLit {} -> Name "storeArrayLit"
        _ -> QualName (Name "Assign") "assign"
  pure $ StmtExp $ Call Nothing fun [lhs', rhs']

transRhs :: (HasCallStack) => NmExp -> CEM NmExp
transRhs expr@(FieldAccess Nothing x) cenv
  | isLocal x cenv = traces ["Local:", pretty x] (Var x)
  | Just _fty <- askFieldTy x cenv =
      let cname = ceName cenv
          cxt = contractContext cenv
          -- rvalFun = Name "rval"
          fieldSel = Con (selectorNameForField cname x) []
          fieldMap = Con "MemberAccessProxy" [cxt, fieldSel]
          result = rhsAccess fieldMap
       in traces ["< transRhs", pretty expr, "~>", pretty result] result
-- Field read on a struct-typed contract (storage) field, e.g. `p.x`. Rather
-- than load the whole struct and project (which touches every field's slot),
-- build a member-access chain that reaches only field x's slot:
--
--   RVA.acc(MemberAccessProxy(LVA.acc(<p as storage(Point)>), Point_x_ssel))
--
-- The inner LVA.acc yields the struct's storage handle; the outer proxy adds
-- StorageSize(offset) to reach the field and RVA.acc loads just that slot.
transRhs (FieldAccess (Just (FieldAccess Nothing p)) field) cenv
  | not (isLocal p cenv),
    Just fty <- askFieldTy p cenv,
    Just structName <- tyConNameOf fty,
    Just fieldNames <- Map.lookup structName cenv.ceStructs,
    field `elem` fieldNames =
      rhsAccess (structMemberProxy p structName field cenv)
-- Struct / member field access `e.n` (receiver present) is NOT a contract-field
-- read: leave the node in place (recursing into the receiver so any contract
-- field inside it is still desugared) so the type checker can lower it to the
-- struct field projection. See Desugarer.StructProjection / TcStmt.
transRhs (FieldAccess (Just e) n) cenv = FieldAccess (Just (transRhs e cenv)) n
transRhs expr@(FieldAccess Nothing _) _ = notImplemented "transRhs" expr
transRhs expr cenv = go expr cenv
  where
    go e@(Indexed arr idx) = \env -> let e' = rhsIndex arr idx env in traces ["transRhs", pretty e, "- rhsIndex ->", pretty e'] e' -- FIXME
    go (Call me f as) = Call me f <$> mapM transRhs as
    go (Lam ps b mty) = Lam ps <$> transBody b <*> pure mty
    go (TyExp e ty) = TyExp <$> transRhs e <*> pure ty
    go (Cond e1 e2 e3) = Cond <$> transRhs e1 <*> transRhs e2 <*> transRhs e3
    go (ArrayLit es) = ArrayLit <$> mapM transRhs es
    go e@Var {} = pure e
    -- Recurse into constructor arguments: a field access can sit inside a
    -- constructor application in r-value position, e.g. a tuple literal
    -- `(this.branch[i], node)` (which parses to `pair(this.branch[i], node)`).
    -- Without this, such nested field accesses reach the type checker
    -- undesugared and fail with "tcExp not implemented for: this.<field>".
    go (Con n es) = Con n <$> mapM transRhs es
    go e@Lit {} = pure e
    go FieldAccess {} = error "Impossible"

indexFun :: Either () () -> Name
indexFun Left {} = (Name "lidx")
indexFun Right {} = (Name "ridx")

indexAccess :: (HasCallStack) => Either () () -> NmExp -> NmExp -> CEM (Exp Name)
indexAccess dir exp@(FieldAccess Nothing name) idx = traces ["iA FA: " ++ pretty name ++ " " ++ pretty idx] $ do
  isF <- isField name
  if isF
    then do
      arrProxy <- memberProxyFor name
      let arrRef = lhsAccess arrProxy
      idx' <- transRhs idx
      pure $ Call Nothing (indexFun dir) [arrRef, idx']
    else collectionIndex dir exp idx
indexAccess dir _exp@(Indexed arr1 idx1) idx2 = traces ["iA II:", pretty arr1, pretty idx1, pretty idx2] $ do
  idx2' <- traces ["transRhs", pretty idx2] $ transRhs idx2
  idx1' <- traces ["transRhs", pretty idx1] $ transRhs idx1
  arr' <- traces ["lhsIndex", pretty arr1, pretty idx1'] $ lhsIndex arr1 idx1'
  pure $ Call Nothing (indexFun dir) [arr', idx2']
indexAccess dir exp idx = collectionIndex dir exp idx

-- Index a collection that is already an ordinary value, e.g. a local of type
-- storage(array(t)) or a function parameter. Unlike a contract field, such a
-- base needs no LVA.acc to synthesise a storage reference: the value is the
-- reference that lidx / ridx consume. A base that is not indexable simply
-- fails to satisfy LValueIdxAccess / RValueIdxAccess during type checking.
collectionIndex :: (HasCallStack) => Either () () -> NmExp -> NmExp -> CEM (Exp Name)
collectionIndex dir exp idx = do
  exp' <- transRhs exp
  idx' <- transRhs idx
  pure $ Call Nothing (indexFun dir) [exp', idx']

lhsIndex, rhsIndex :: (HasCallStack) => NmExp -> NmExp -> CEM (Exp Name)
lhsIndex = indexAccess $ Left ()
rhsIndex = indexAccess $ Right ()

--------------------------------
-- # Helpers
--------------------------------

askFieldTy :: Name -> ContractEnv -> Maybe Ty
askFieldTy x env = fieldTy <$> Map.lookup x env.ceFields

-- | Head type-constructor name of a type, if it is a constructor application.
tyConNameOf :: Ty -> Maybe Name
tyConNameOf (TyCon n _) = Just n
tyConNameOf _ = Nothing

isField :: Name -> ContractEnv -> Bool
isField x env = isJust $ askFieldTy x env

isLocal :: Name -> ContractEnv -> Bool
isLocal x env = Set.member x env.ceLocals

getFields :: [NmContractDecl] -> [NmField]
getFields cdecls = concatMap getF cdecls
  where
    getF (CFieldDecl f) = [f]
    getF _ = []

appendToName :: Name -> String -> Name
appendToName (Name s) t = Name (s <> t)
appendToName (QualName n s) t = QualName n (s <> t)

selectorNameForField :: Name -> Name -> Name
selectorNameForField cname (Name fld) = Name (show cname <> "_" <> fld <> "_sel")
selectorNameForField _ n = notImplementedS "selectorNameForField" n

-- Selector data-type name for a *struct* field (member access into a storage
-- struct). Distinct suffix from 'selectorNameForField' so a struct and a
-- contract that share a field name never produce clashing selector types.
structFieldSelName :: Name -> Name -> Name
structFieldSelName structTy (Name fld) = Name (show structTy <> "_" <> fld <> "_ssel")
structFieldSelName _ n = notImplementedS "structFieldSelName" n

singletonNameForContract :: Name -> Name
singletonNameForContract cname = appendToName cname "Cxt"

singletonTypeForContract :: Name -> Ty
singletonTypeForContract cname = TyCon (singletonNameForContract cname) []

-- singletonValForContract :: Name -> NmExp
-- singletonValForContract cname = Con (singletonNameForContract cname) []

contractContext :: CEM NmExp
contractContext = do
  cname <- reader ceName
  let singName = singletonNameForContract cname
  -- let contractSingTy = TyCon singName []
  let contractSing = Con singName []
  -- let cxtTy = TyCon "ContractStorage" [contractSingTy]
  let cxt = Con "ContractStorage" [contractSing]
  pure cxt

memberProxyFor :: Name -> CEM NmExp
memberProxyFor field = do
  cname <- reader ceName
  cxt <- contractContext
  let selName = selectorNameForField cname field
  let selector = Con selName []
  let fieldMap = Con "MemberAccessProxy" [cxt, selector]
  pure fieldMap

-- Member-access proxy reaching struct field @field@ of contract storage field
-- @p@ (a value of struct type @structName@). The proxy's base is the struct's
-- own storage handle, obtained by taking the l-value of the contract field
-- (@LVA.acc@ of the contract-field proxy); the outer proxy pairs that handle
-- with the struct-field selector so the std struct-in-storage LVA/RVA instances
-- compute @base + StorageSize(offset)@ for just this field.
structMemberProxy :: Name -> Name -> Name -> ContractEnv -> Exp Name
structMemberProxy p structName field cenv =
  Con "MemberAccessProxy" [structBase, selector]
  where
    structBase = lhsAccess (memberProxyFor p cenv)
    selector = Con (structFieldSelName structName field) []

lhsAccess :: Exp Name -> Exp Name
lhsAccess e = Call Nothing (QualName "LVA" "acc") [e]

rhsAccess :: Exp Name -> Exp Name
rhsAccess e = Call Nothing (QualName "RVA" "acc") [e]

notImplemented :: (HasCallStack, Pretty a) => String -> a -> b
notImplemented funName a = error $ concat [funName, " not implemented yet for ", pretty a]

notImplementedS :: (HasCallStack, Show a) => String -> a -> b
notImplementedS funName a = error $ concat [funName, " not implemented yet for ", show (pShow a)]

traces :: [String] -> a -> a
-- traces = trace . unwords
traces _ a = a
