-- | Regression tests for 'Language.Hull.Compress' (the @yule -O@ sum-compression pass).
--
-- The pass rewrites the binary @inl@/@inr@ encoding of an N-ary sum into the
-- compressed @in(k)@ (EInK) form. The tricky case is a value whose arm carries
-- another sum: that nested payload must itself be recompressed, otherwise its
-- inner injections stay in binary @inl@/@inr@ form while the surrounding match is
-- rewritten to @in(k)@, and @Translate.sizeOf@ disagrees with the consumer's slot
-- count -- a silent miscompilation.
--
-- The reported trigger:
--
-- >  data Inner = A() | B() | C();
-- >  data Outer = X(Inner) | Y() | Z();
-- >  let o : Outer = Outer.X(Inner.C());   -- inner match on i picked the wrong arm
--
-- @Inner.C()@ encodes as binary @inr(inr(()))@; after compression it must become
-- @in(2)@. Before the fix, the @EInl@ arm of @compressInjections.go@ attached the
-- payload without recompressing it, so @Outer.X(Inner.C())@ compressed to
-- @in(0)(inr(inr(())))@ instead of @in(0)(in(2)(()))@.
module CompressTests (compressTests) where

import Language.Hull
import Language.Hull.Compress (compress)
import Test.Tasty
import Test.Tasty.HUnit

-- Inner = A() | B() | C()  -- a 3-way sum, binary nesting A + (B + C)
innerTy :: Type
innerTy = TNamed "Inner" (TSum TUnit (TSum TUnit TUnit))

-- Inner.C() = inr(inr(())) -- tag 2, the last arm
innerC :: Expr
innerC = EInr innerTy (EInr (TSum TUnit TUnit) EUnit)

-- | @isInK k p e@ holds when @e@ is @EInK k _ payload@ and @p payload@ holds.
isInK :: Int -> (Expr -> Bool) -> Expr -> Bool
isInK k p (EInK k' _ payload) = k == k' && p payload
isInK _ _ _ = False

isUnit :: Expr -> Bool
isUnit EUnit = True
isUnit _ = False

-- | Compress @e@ and assert the result matches the expected EInK shape.
shapeCase :: String -> (Expr -> Bool) -> Expr -> TestTree
shapeCase name expected e =
  testCase name $
    let got = compress e
     in if expected got
          then pure ()
          else assertFailure ("unexpected compressed shape: " ++ show got)

compressTests :: TestTree
compressTests =
  testGroup
    "Sum compression (yule -O)"
    [ testGroup
        "flat 3-way sum maps each arm to its tag"
        [ -- Inner.A() = inl(())                 -> in(0)(())
          shapeCase "A -> in(0)" (isInK 0 isUnit) (EInl innerTy EUnit),
          -- Inner.B() = inr(inl(()))            -> in(1)(())
          shapeCase "B -> in(1)" (isInK 1 isUnit) (EInr innerTy (EInl (TSum TUnit TUnit) EUnit)),
          -- Inner.C() = inr(inr(()))            -> in(2)(())
          shapeCase "C -> in(2)" (isInK 2 isUnit) innerC
        ],
      testGroup
        "nested sum payload is recompressed (the regression)"
        [ -- Outer = X(Inner) | Y() | Z(); Outer.X(Inner.C()) reached via the inl arm.
          -- Bug: payload stayed inr(inr(())); fix: in(0)(in(2)(())).
          shapeCase
            "Outer.X(Inner.C()) -> in(0)(in(2)())"
            (isInK 0 (isInK 2 isUnit))
            (EInl (TNamed "Outer" (TSum innerTy (TSum TUnit TUnit))) innerC),
          -- Outer2 = X() | Y(Inner) | Z(); Outer2.Y(Inner.C()) reached after peeling one inr.
          shapeCase
            "Outer2.Y(Inner.C()) -> in(1)(in(2)())"
            (isInK 1 (isInK 2 isUnit))
            ( EInr
                (TNamed "Outer2" (TSum TUnit (TSum innerTy TUnit)))
                (EInl (TSum innerTy TUnit) innerC)
            ),
          -- Outer3 = X() | Y() | Z(Inner); Outer3.Z(Inner.C()) is the last arm (the guard clause).
          shapeCase
            "Outer3.Z(Inner.C()) -> in(2)(in(2)())"
            (isInK 2 (isInK 2 isUnit))
            ( EInr
                (TNamed "Outer3" (TSum TUnit (TSum TUnit innerTy)))
                (EInr (TSum TUnit innerTy) innerC)
            )
        ]
    ]
