module ContractDispatchTests (contractDispatchTests) where

import Data.List (isInfixOf)
import Solcore.Desugarer.ContractDispatch (contractDispatchDesugarer)
import Solcore.Frontend.Pretty.SolcorePretty (pretty)
import Solcore.Frontend.Syntax
import Test.Tasty
import Test.Tasty.HUnit

-- A minimal contract whose constructor takes (typed) arguments. The generated
-- `copy_arguments_for_constructor` routine only emits the argument-copy assembly
-- (the branch carrying the codesize-underflow guard) when the constructor
-- actually has parameters; a zero-arg constructor short-circuits to `return ()`.
contractWithConstructorArgs :: CompUnit Name
contractWithConstructorArgs =
  CompUnit
    []
    [ TContr
        Contract
          { name = Name "C",
            tyParams = [],
            decls =
              [ CConstrDecl
                  Constructor
                    { constrParams = [Typed False (Name "x") (TyCon (Name "word") [])],
                      constrBody = [],
                      constrPayable = False
                    }
              ]
          }
    ]

-- The pretty-printed dispatch code generated for the contract above.
generated :: String
generated = pretty (contractDispatchDesugarer contractWithConstructorArgs)

contractDispatchTests :: TestTree
contractDispatchTests =
  testGroup
    "Contract dispatch"
    -- Regression guard for the constructor argSize underflow: a truncated
    -- init-code (codesize() < programSize) would make `sub` wrap to ~2^256 and
    -- corrupt the free-memory pointer / codecopy length. We can't exercise this
    -- behaviourally (the testrunner always deploys the full bytecode, and the
    -- pre-fix code also reverts on truncation via an out-of-gas codecopy), so we
    -- assert structurally that the generated constructor keeps the guard.
    [ testCase "constructor arg copy guards against codesize() underflow" $ do
        assertBool
          ("expected a lt(codesize(), programSize) guard in generated code:\n" ++ generated)
          ("lt(codesize" `isInfixOf` generated)
        assertBool
          ("expected the underflow guard to revert in generated code:\n" ++ generated)
          ("revert" `isInfixOf` generated)
    ]
