module CheckTest where

import Test.Tasty (testGroup)
import Test.Tasty.HUnit (assertEqual, assertBool, testCase)
--import Test.Tasty.QuickCheck 

import Ast
import Check

-- provide tests that show your check works

tests = testGroup "CheckTest"
  [
  testCase "Check the format of expressions" $
    do
      assertEqual "let x = 1 in x + 1" [] $ check $ Let (Var "x") (ValInt 1) (Plus (Var "x") (ValInt 1))
      assertEqual "\\x -> x + 1" [] $ check $ Lam (Var "x") (Plus (Var "x") (ValInt 1))
      assertEqual "let x = 0 in (let y = x in x)" [] $ check $ Let (Var "x") (ValInt 0) (Let (Var "y") (Var "x") (Var "x"))
      assertEqual "\\x -> \\x -> x" [] $ check $ Lam (Var "x") (Lam (Var "x") (Var "x"))
      assertEqual "let x = 1 in (let x = True in x)" [] $ check $ Let (Var "x") (ValInt 1) (Let (Var "x") (ValBool True) (Var "x"))
      assertEqual "\\x -> (let x = [x, x] in x)" [] $ check $ Lam (Var "x")(Let (Var "x") ((Var "x") `cons` (Var "x") `cons` Nil) (Var "x"))
      assertEqual "let x = 0 in (\\x -> x)" [] $ check $ Let (Var "x") (ValInt 0) (Lam (Var "x") (Var "x"))
      assertEqual "\\x -> \\y -> x" [] $ check $ Lam (Var "x") (Lam (Var "y") (Var "x"))
      assertEqual "let x = 0 in (let y = 0 in x)" [] $ check $ Let (Var "x") (ValInt 0) (Let (Var "y") (ValInt 0) (Var "x"))
      assertEqual "let x = 0 in (\\y -> x)" [] $ check $ Let (Var "x") (ValInt 0)(Lam (Var "y") (Var "x"))
      assertEqual "\\x -> (let y = (\\x -> x) in x)" [] $ check $ Lam (Var "x")(Let (Var "y") (Lam (Var "x") (Var "x"))(Var "x"))
      assertEqual "\\x -> y" (UndefinedVarUse y) $ check $ Lam (Var "x") (Var "y")
      assertEqual "\\x -> y + z" (Set.fromList [UndefinedVarUse "y", UndefinedVarUse "z"]) $ check $ Lam (Var "x") (Plus (Var "y") (Var "z"))
      assertEqual "let x = 1 in x + z" (UndefinedVarUse "z") $ check $ Let (Var "x") (ValInt 1) (Plus (Var "x")(Var "z"))
      assertEqual "let x = 0 in y + z" (Set.fromList [UndefinedVarUse "y", UndefinedVarUse "z"]) $ check $ Let (Var "x") (ValInt 0) (Plus (Var "y")(Var "z"))
  ]

{-

--   * use of undefined variable
--   * defined but unused variable
--   * type errors

-- reference in expression
\x -> x + 1  
let x = 1 in x + 1   

-- reference in let binding
let x = 0 in (let y = x in x)  

-- reuse of reference (depending on your implementation, you can force this not to be valid)
\x -> \x -> x  
let x = 1 in (let x = True in x)
\x -> (let x = [x, x] in x)
let x = 0 in (\x -> x)

-- nested reference
\x -> \y -> x
let x = 0 in (let y = 0 in x)
let x = 0 in (\y -> x)
\x -> (let y = (\x -> x) in x)

-- longer variable name
\x -> x1
\x -> \y -> xy

-- not in the correct scope 
let x = y in (\y -> x)
let y = 0 in x + (\x -> x)






-}

