module EvalTest where

import Test.Tasty (testGroup)
import Test.Tasty.HUnit (assertEqual, assertBool, testCase)
--import Test.Tasty.QuickCheck 
import Test.Tasty.QuickCheck hiding (Fun)
import qualified Test.Tasty.QuickCheck as QC (Fun(..))
import qualified Data.Map as Map
import EnvUnsafelog (runEnvUnsafelog, Unsafe(..), EnvUnsafelog(..))
import Ast
import qualified Eval as L0 (Ast(ValChar,ValInt,ValDouble,ValBool,Plus,And,Or,Not,Minus), eval)

-- provide tests that show your run/eval works

data TestVal = TI Integer | TB Bool | TF Double | TC Char
        | TLs [TestVal] deriving (Eq, Show)

 -- | an unsafe function to convert Val to TestVal
  valToTestVal :: Val -> TestVal 
  valToTestVal (I n) = TI n 
  valToTestVal (B b) = TB b
  valToTestVal (F d) = TF d
  valToTestVal (C c) = TC c 
  valToTestVal (Ls lst) = TLs $ map valToTestVal lst 
  valToTestVal (Lang.Fun _) = error "unexpected function construction"


  -- | Test if two Unsafe Int val is equal 
  eqInt :: Unsafe Val -> Unsafe Val -> Bool 
  eqInt (Ok (I n)) (Ok (I m)) = m == n 
  eqInt _ _ = False 

  -- | Test if two Unsafe Boolean is equal 
  eqBool :: Unsafe Val -> Unsafe Val -> Bool 
  eqBool (Ok (B b1)) (Ok (B b2)) = b1 == b2 
  eqBool _ _ = False


    -- | Test if two Unsafe Char val is equal 
  eqChar :: Unsafe Val -> Unsafe Val -> Bool 
  eqChar (Ok (C n)) (Ok (C m)) = m == n 
  eqChar _ _ = False 

  -- | Test if two Unsafe Douuble is equal 
  eqDouble :: Unsafe Val -> Unsafe Val -> Bool 
  eqDouble (Ok (F b1)) (Ok (F b2)) = b1 == b2 
  eqDouble _ _ = False

  eqLs :: Unsafe Val -> Unsafe Val -> Bool
  eqLs (Ok (Ls a)) (Ok (Ls b)) = a == b
  eqLs _ _ = False

  -- | Apply a unsafe function
  appFunc :: Unsafe Val -> Unsafe Val -> Unsafe Val 
  appFunc (Error msg) _ = (Error msg)
  appFunc _ (Error msg) = (Error msg)
  appFunc (Ok (Fun f)) (Ok val) = f val 
  appFunc (Ok val) _ = Error "first input is not a function in appFunc"

  -- | test if an unsafe is error
  isError :: Unsafe a -> Bool 
  isError (Error _) = True 
  isError _ = False 

  -- | return if the two unsafe has the same error
  sameError :: Unsafe a -> Unsafe b -> Bool 
  sameError (Error msg1) (Error msg2) = msg1 == msg2
  sameError _ _ = False

  -- | run a program under empty exprssion
  runEmpty :: Ast -> Unsafe Val 
  runEmpty exp = runEnvUnsafe (eval exp) Map.empty
  runEmptyT :: Ast -> Unsafe TestVal 
  runEmptyT exp = 
    case runEnvUnsafe (eval exp) Map.empty of 
      Ok res -> Ok $ valToTestVal res 
      Error msg -> Error msg

   -- | Get the subexpression of the ast 
  subexpression :: Ast -> [Ast] 
  subexpression (ValBool _) = []
  subexpression (ValDouble _) = []
  subexpression (ValChar _) = []
  subexpression (And r l) = [r, l]
  subexpression (Or r l) = [r, l]
  subexpression (Not inp) = [inp]
  subexpression (ValInt _) = []
  subexpression (Plus r l) = [r, l]
  subexpression (Minus r l) = [r, l]
  subexpression (Mult r l) = [r, l]
  subexpression (Div r l) = [r, l]
  subexpression (Nil) = []
  subexpression (Cons r l) = [r, l]
  subexpression (If b trueExp falseExp) = [b, trueExp, falseExp]
  subexpression (Let varName varExp bodyExp) = [varExp, bodyExp]
  subexpression (Var _) = []
  subexpression (Lam para body) = [body]
  subexpression (App f x) = [f, x]




   instance Arbitrary Ast where
    arbitrary = sized arbitrarySizedAst

  arbitrarySizedAst ::  Int -> Gen Ast
  arbitrarySizedAst m | m < 1 = 
    do 
      i <- arbitrary
      b <- arbitrary
      c <- arbitrary
      d <- arbitrary
      node <- elements [ValInt i, ValBool b, ValDouble d, ValChar c, Nil]
      return $ node
  arbitrarySizedAst m | otherwise = 
    do 
      l <- arbitrarySizedAst (m `div` 2)
      r <- arbitrarySizedAst (m `div` 2)
      str <- elements ["x","y","z"]
      ifast <- arbitrarySizedIf m
      node <- elements [And l r, Or l r, Not l,
                        Plus l r, Minus l r, Mult l r, Div l r,
                        Cons l r,
                        ifast,
                        Let str l r,
                        Lam str l,
                        App l r,
                        Var str
                        ]
      return node

 -- it would be better if every branch were built like this so the balance would be maintained
  arbitrarySizedIf ::  Int -> Gen Ast
  arbitrarySizedIf m = do b <- arbitrarySizedAst (m `div` 3)
                          t <- arbitrarySizedAst (m `div` 3)
                          e <- arbitrarySizedAst (m `div` 3)
                          return $ If b t e

  -- declear cons as a right associative infix 
  infixr 7 `cons`
  cons = Cons

  -- | test for if the result carries the error
  errorTest = testGroup "test for basic error handling" [
    -- testProperty "for all ast (except function, because they can be lazy), the eval result should have the same error as the first error encountered" $
    --   \ast -> 
    --     let 
    --       subExpsRes = map (\exp -> runEnvUnsafe (eval exp) Map.empty) $ subexpression ast 
    --       subExpErrors = filter isError subExpsRes
    --       finalRes = runEnvUnsafe (eval ast) Map.empty 
    --     in 
    --       not (null subExpErrors) ==>
    --         case finalRes of 
    --           Ok (Fun _) -> True  -- do not test function
    --           _ ->  sameError (head subExpErrors) finalRes,
   testCase "type mismatch example" $
      do
      	assertBool "3.0 - c" $ isError(runEmptyT $ ValDouble 3.0  `Minus` ValChar 'c')
      	assertBool "3.0 - 1" $ isError(runEmptyT $ ValDouble 3.0  `Minus` ValInt 1)
      	assertBool "3.0 + c" $ isError(runEmptyT $ ValDouble 3.0  `Plus` VarChar 'c')
        assertBool "3.0 + 1" $ isError(runEmptyT $ ValDouble 3.0  `Plus` ValInt 1)
        assertBool "3.0 + True" $ isError (runEmptyT $ ValDouble 3.0 `Plus` ValBool True)
        assertBool "1 + True" $ isError (runEmptyT $ ValInt 1 `Plus` ValBool True)
        assertBool "[] - 2" $ isError (runEmptyT $ Nil `Minus` ValInt 2)
        assertBool "1 and True" $ isError (runEmptyT $ ValInt 1 `And` ValBool True)
        assertBool "1.0 and 'c'" $ isError (runEmptyT $ ValDouble 1.0 `And` ValChar 'c')
        assertBool "[] or False" $ isError (runEmptyT $ Nil `Or` ValBool False)
        assertBool "2.0 or 'd'" $ isError (runEmptyT $ ValDouble 2.0 `And` ValChar 'd')
        assertBool "not []" $ isError (runEmptyT $ Not Nil)
        assertBool "Cons [] 4.0" $ isError (runEmptyT $ Cons Nil (ValDouble 4.0))
        assertBool "Cons [] 'c'" $ isError (runEmptyT $ Cons Nil (ValChar 'c'))
        assertBool "Cons [] 3" $ isError (runEmptyT $ Cons Nil (ValInt 3))
        assertBool "if [] then 4.0 else 5.0" $ isError (runEmptyT $ If Nil (ValDouble 4.0) (ValDouble 5.0))
        assertBool "if [] then 2 else 3" $ isError (runEmptyT $ If Nil (ValInt 2) (ValInt 3))
        assertBool "App 1 2" $ isError (runEmptyT $ (ValInt 1) `App` (ValInt 2))
        assertBool "App 4.0 5.0" $ isError (runEmptyT $ (ValDouble 4.0) `App` (ValDouble 5.0))
        
    ]
  
   tests = testGroup "Evaltest" [
    testCase "evaluation should support multityped list" $ 
      do
        assertEqual "[1, 2.0, True, 'c']" (Ok $ TLs [TI 1, TF 2.0, TB True, TC 'c']) 
          (runEmptyT $ ValInt 1 `cons` ValDouble 2.0 `cons` ValBool True `cons`ValChar 'c' `cons` Nil)
        assertEqual "[1, 2, True]" (Ok $ TLs [TI 1, TI 2, TB True]) 
          (runEmptyT $ ValInt 1 `cons` ValInt 2 `cons` ValBool True `cons` Nil)
        assertEqual "[1, True, 2.0, 'c',[False, 2,4.0,'d']]" (Ok $ TLs [TI 1, TB True, TF 2.0, TC 'c', TLs [TB False, TI 2, TF 4.0, TC 'd']])
          (runEmptyT $ ValInt 1 `cons` ValBool True `cons` ValDouble 2.0 `cons` ValChar 'c' `cons` (ValBool False `cons` ValInt 2 `cons` ValDouble 4.0 `cons` ValChar 'd' `cons` Nil) `Cons` Nil) 
        assertEqual "[1, True, [False, 2]]" (Ok $ TLs [TI 1, TB True, TLs [TB False, TI 2]]) 
          (runEmptyT $ ValInt 1 `cons` ValBool True `cons` (ValBool False `cons` ValInt 2 `cons` Nil) `Cons` Nil),
    
    testCase "division by 0" $
      assertBool "1.0/0.0" $ isError (runEmptyT $ ValDouble 1.0 `Div` ValDouble 0.0)
      assertBool "1/0" $ isError (runEmptyT $ ValInt 1 `Div` ValInt 0)

    
    testProperty "function in list example: [True, 1, \\x -> x]" $
      \inp -> 
        let 
          res = runEmpty $ ValBool True `cons` ValInt 1 `cons` Lam "x" (Var "x") `cons` Nil
        in 
          case res of 
            Ok (Ls [B True, I 1, Fun f]) -> (f $ I inp) `eqInt` (Ok $ I $ inp)
            _ -> False,
    
    testProperty "nested function: \\x -> \\y -> x + y" $
      \x y ->
        let 
          res =  runEmpty $ Lam "x" $ Lam "y" $ (Var "x") `Plus` (Var "y")
        in 
          (res `appFunc` Ok (I x) `appFunc` Ok (I y)) `eqInt` (Ok $ I $ x + y),
    
    testProperty "function as input: \\f -> \\x -> f x" $ 
      \(QC.Fun _ f :: QC.Fun Integer Integer) n -> 
        let 
          res =  runEmpty $ Lam "f"  $ Lam "x" $ (Var "f") `App` (Var "x")
          fVal inp = 
            case inp of 
              I n -> Ok $ I $ f n 
              _ -> Error "f only handles Integer"
        in 
          (res `appFunc` Ok (Fun fVal) `appFunc` Ok (I n)) `eqInt` (Ok $ I $ f n)
          
    ]
 
  operatorTest = testGroup " test for each operator"[
     separatortest,
     booleanortest,
     booleanandtest,
     equaltest,
     notequaltest,
     lessthantest,
     less_than_or_equal_test,
     greater_than_or_equaltest,
     greater_than_test,
     listtest,
     list_concatenatationtest,
     additiontest,
     subtractionstest,
     multiplicationtest,
     floating_point_division_test,
     integer_division_test,
     modulus_test,
     floating_point_exp_test,
     integer_exp_test,
     list_indexing_test,
     boolean_not_test,
     unarminustest,
     printtest,
     compoundtest,
     iftest,
     lettest,
     apptest,
     lamtest
  ]

separatortest = testCase "test for separator" $
                    do
                      assertEqual [] (Ok $ I 2, []) $ run $ (L0.ValInt 3) `L0.Separator` (L0.ValInt 2)
booleanortest = testCase "test for boolean_or" $
                   do  
                     assertEqual [] (Ok $ B True, []) $ run $ (L0.ValBool True) `L0.Or` (L0.ValBool False)
booleanandtest = testCase "test for boolean_and" $
                    do     
                      assertEqual [] (Ok $ B True, []) $ run $ (L0.ValBool True, []) `L0.And` (L0.ValBool True)
equaltest = testCase "test for equal” $
              do
                assertEqual [] (Ok $ B True,[]) $ run $ (L0.ValBool True) `L0.Eq` (L0.ValBool True)
                assertEqual [] (OK $ B True,[]) $ run $ (L0.(ValInt 1 `cons` ValDouble 2.0 `cons` Nil)) `L0.Eq` (L0.(ValInt 1 `cons` ValDouble 2.0 `cons` Nil)
                assertEqual [] (Ok $ B True,[]) $ run $ (L0.ValDouble 2.0) `L0.Eq` (L0.ValDouble 2.0)
notequaltest = testCase "test for notequal" $
                  do
                    assertEqual [] (Ok $ B False,[]) $ run $ (L0.ValBool True) `L0.NEq` (L0.ValBool False) 
                    assertEqual [] (Ok $ B False,[]) $ run $ (L0.(ValInt 1 `cons` ValDouble 2.0)) `L0.NEq` (L0.(ValInt 2 `cons` ValDouble 4.0))
                    assertEqual [] (Ok $ B True,[]) $ run $ (L0.ValDouble 2.0) `L0.NEq` (L0.ValDouble 4.0)
lessthantest = testCase "test for less than" $
                  do
                    assertEqual [] (Ok $ B True,[]) $ run $ (L0.ValDouble 2.0) `L0.Lt` (L0.ValDouble 4.0)
                    assertEqual [] (Ok $ B True,[]) $ run $ (L0.ValInt 2) `L0.Lt` (L0.ValInt 4)
less_than_or_equal_test = testCase "test for less_than_or_equal" $
                      do
                        assertEqual [] (Ok $ B True,[]) $ run $ (L0.ValDouble 4.0) `L0.Le` (L0.ValDouble 4.0)
                        assertEqual [] (Ok $ B True,[]) $ run $ (L0.ValInt 2) `L0.Le` (L0.ValInt 3)
greater_than_test = testCase "test for greater_than" $
                        do
                          assertEqual [] (Ok $ B False,[]) $ run $ (L0.ValDouble 4.0) `L0.Gt` (L0.ValDouble 5.0)
                          assertEqual [] (Ok $ B True,[]) $ run $ (L0.ValInt 5) `L0.Gt` (L0.ValInt 6)
greater_than_or_equaltest = testCase "test for greater_than_or_equal" $
                  do
                    assertEqual [] (Ok $ B True,[]) $ run $ (L0.ValDouble 5.0) `L0.Ge` (L0.ValDouble 5.0)
                    assertEqual [] (Ok $ B True,[]) $ run $ (L0.ValInt 5) `L0.Ge` (L0.ValInt 6)
listtest = testCase "test for list" $
              do
                assertEqual [] (Ok $ Ls $ [F 2.0, I 2,B True],[]) $ run $ (L0.ValDouble 2.0) `L0.Cons` (L0.ValInt 2) `L0.Cons` (L0.ValBool True) `L0.Cons` (L0.Nil)

list_concatenatationtest = testCase "test for list_concatenatation" $
                              do
                                assertEqual [] (Ok $  Ls $ [F 2.0, I 2, B True],[]) $ run $ ((L0.ValDouble 2.0) `cons` (L0.ValInt 2) `cons` (L0.Nil)) `L0.Concat` ((L0.ValDouble True) `cons` Nil)

additiontest = testCase "test for addition" $
                  do
                    assertEqual [] (Ok $ F 2.0,[]) $ run $ (L0.ValDouble 1.0) `L0.PLus` (L0.ValDouble 1.0)
                    assertEqual [] (Ok $ I 2 ,[]) $ run $ (L0.ValInt 1) `L0.PLus` (L0.ValInt 1)

subtractionstest = testCase "test for subtractions" $
                      do
                        assertEqual [] (Ok $ F 3.0,[]) $ run $ (L0.ValDouble 4.0) `L0.Sub` (L0.ValDouble 1.0)
                        assertEqual [] (Ok $ I 3,[]) $ run $ (L0.ValInt 4) `L0.Sub` (L0.ValInt 1)
multiplicationtest = testCase "test for multiplication" $
                        do      
                          assertEqual [] (Ok $ F 4.0,[]) $ run $ (L0.ValDouble 2.0) `L0.Mult` (L0.ValDouble 2.0)
                          assertEqual [] (Ok $ I 4, []) $ run $ (L0.ValInt 2) `L0.Mult` (L0.ValInt 2)

floating_point_division_test = testCase "test for floating_point_division" $
                                  do      
                                    assertEqual [] (Ok $ F 1.0,[]) $ run $ (L0.ValInt 2.0) `L0.DoubleDiv` (L0,ValDouble 2.0)

integer_division_test = testCase "test for integer_division" $
                            do            
                              assertEqual [] (Ok $ I 1,[]) $ run $ (L0.ValInt 1) `L0.IntDiv` (L0.ValInt 1)
modulus_test = testCase "test for modulus" $
                  do
                    assertEqual [] (Ok$ I 5,[]) $ run $ (L0.ValInt 5) `L0.Mod` (L0.ValInt 4) 
floating_point_exp_test = testCase "test for floating_point_exp" $
                              do
                                assertEqual [] (Ok $F 4.0,[]) $ run $ (L0.ValDouble 2.0) `L0.Fe` (L0.ValDouble 2.0)  
integer_exp_test =  testCase "test for integer_exp" $
                       do   
                          assertEqual [] (Ok $I 4,[]) $ run $ (L0.ValInt 2) `L0.Ie` (L0.ValInt 2)
list_indexing_test =  testCase "test for list_indexing" $
                         do    
                           assertEqual [] (Ok $ I 2, []) $ run $ (L0.ValInt 1) `L0.Li` (L0.((ValInt 1) `cons` (ValInt 2) `cons` (ValBool True) `cons`Nil))

boolean_not_test = testCase "test for boolean_not " $
                      do      
                        assertEqual [] (Ok $ B True,[]) $ run $ L0.Not (L0.ValBool False)
unarminustest = testCase "test for unarminustest" $
                   do        
                     assertEqual [] ( OK $ I (-2), []) $ run $ L0.Ne (L0.ValInt 2)
printtest = testCase "test for print" $
                do      
                  assertEqual [] ( OK $ I 2, []) $ run $ L0.Print (L0.ValInt 2)

compoundtest = testCase "Compound Arithmetic" $
                  do 
                    assertEqual "2 + 4 * 3 =? " (Ok $ I 14,[]) $  run (L0.Plus (L0.ValInt 2) (L0.Mult (L0.ValInt 4) (L0.ValInt 3))
                    assertEqual "(2 + -4) * 3 =? "   (Ok $ I (-6), []) $  run (L0.Mult (L0.Plus (L0.ValInt 2) (L0.Ne(L0.ValInt 4))) (L0.ValInt 3))
                    assertEqual "2 * 3 + 3 * 2 - 4 =? "     (OK $ I 8,[]) $ run (L0.Sub (L0.Plus (L0.Mult (L0.ValInt 2) (L0.ValInt 3)) (L0.Mult (L0.ValInt 3) (L0.ValInt 2))) (L0.ValInt 4))
                    assertEqual "2 * (3 + 3) * (2 - 4) =? " (Ok $ I (-24),[]) $ run (L0.Mult (L0.Mult (L0.ValInt 2) (L0.Plus (L0.ValInt 3) (L0.ValInt 3)) (L0.Sub (L0.ValInt 2) (L).ValInt 4)))),

iftest = testCase "If Statements" $
            do 
              assertEqual "if 3 then 4 else 2 =? "       (Ok $ I 4,[])  (run (L0.If (L0.ValInt 2) (L0.ValInt 4) (L0.ValInt 2))
              assertEqual "if 0 then 1 else 4"           (Ok $ I 4,[])  (run (L0.If (L0.ValInt 0) (L0.ValInt 1) (L0.ValInt 4))
              assertEqual "if 3 * 0 then 1 else 2  =? "  (Ok $ I 2,[])  (run (L0.If (L0.Mult (L0.ValInt 3) (L0.ValInt 0)) (L0.ValInt 1) (L0.ValInt 2)))
              assertEqual "if 3 * 2 then 1 else 2  =? "  (Ok $ I 1,[])  (run (L0.If (L0.Mult (L0.ValInt 3) (L0.ValInt 2)) (L0.ValInt 1) (L0.ValInt 2))),

lettest = testCase "Let Statements" $
             do 
               assertEqual "let x = 4 in x * 2 =? "                   (Ok $ I 8,[])  (run (L0.Let "x" (L0.ValInt 4) (L0.Mult (Var "x") (L0.ValInt 2))))
               assertEqual "let x = 4 * -2 in x - 2 =? "              (Ok $ I (-10),[])  (run (L0.Let "x" (L0.Mult (L0.ValInt 4) (L0.Ne(L0.ValInt 2))) (L0.Sub (Var "x") (L0.ValInt 2))))
               assertEqual "let x = 2 in let y = x + 1 in y * 2 =? "  (Ok $ I 6,[])  (run (L0.Let "x" (L0.ValInt 2) (L0.Let "y" (L0.Plus (Var "x") (L0.ValInt 1 ))  (L0.Mult (Var "y") (L0.ValInt 2)))))
apptest = testCase "App Statements"
            do 
              assertEqual "app \\x -> x 5" (Ok $ I 5, [])(run (L0.App (L0.Lam ("x")(ValChar "x"))(ValInt 5)))
              assertBool "app x 2"$isError(run L0.App(ValChar "x") (ValInt 2))
              assertEqual "app \\x -> x + 10 5"(Ok $ I 10 ,[])(run(L0.App(L0.Lam("x")(L0.Plus(ValChar "x")(ValInt 10)))(ValInt 5)))
              assertBool "app x x" $isError (run L0.App (ValChar "x")(ValChar "x"))

lamtest = testCase "Lam Statements"
        do 
          assertBool "\\x -> y+5" $isError $run (L0.Lam("x")(L0.Plus (ValChar "y")(ValInt 5))





 stdLibTest = testGroup "test for standard library" [
    testCase "test for tail function" $
      do 
        assertBool "for an empty list, the tail should return an error" $ isError $ run (Var "tail" `App` Nil)
        assertBool "tail of [1, 2] should be 2" $ run (Var "tail" `App` (ValInt 1 `cons` ValInt 2 `cons` Nil)) `eqLs` (Ok $ Ls 2),

    testCase "test for head function" $ 
      do 
        assertBool "for a empty list, the head should return an error" $ isError $ run (Var "head" `App` Nil)
        assertBool "head of [1, \\x -> x] should be 1" $ run (Var "head" `App` (ValInt 1 `cons` Lam "x" (Var "x") `cons` Nil)) `eqInt` (Ok $ I 1),
    
    testCase "test for len function" $ 
      do 
        assertBool "for a empty list, the length shold be 0" $ run (Var "len" `App` Nil) `eqInt` (Ok $ I $ 0)
        assertBool "length of [True, 1, \\x -> x, False] should be 4" $  
          run (Var "len" `App` (ValBool True `cons` ValInt 1 `cons` Lam "x" (Var "x") `cons` ValBool False `cons` Nil)) `eqInt` (Ok $ I 4),
    
 

    testCase "test for elem function" $ 
      do 
        assertBool "for a empty list, the elem function shold return false" $ run (Var "str"`App` Var "elem" `App` Nil) `eqBool` (Ok $ B $ False)
        assertBool "(elem (\y -> 0) [(\x -> x*0)] ) should return an error" $ isError $ run ((Lam "y" (VarInt 0)) `App` Var "elem" `App` ((Lam "x" (Var "x*0") `cons` Nil)))
        assertBool "elem True for list [True, 1, \\x -> x, False] should return true" $  
          run (Var "True" `App` Var "elem" `App` (ValBool True `cons` ValInt 1 `cons` Lam "x" (Var "x") `cons` ValBool False `cons` Nil)) `eqBool` (Ok $ B True),


     testCase "test for map function" $ 
      do 
        assertBool "map (+2) for empty list should return an empty list" $ run (Var "(+2)" `App` Var "map" `App` Nil) `eqBool` (Ok $ TLs [])
        assertBool "for map (+2), the list [1,2,3] should return [3,4,5]" $ run (Var "(+2)" `App` Var "map" `App` (ValInt 1, ValInt 2, ValInt 3)) `eqBool` (Ok $ TLs [TI 3, TI 4, TI 5])
        assertBool "for map (+2.0), the list [1.0,2.0,3.0] should return [3.0,4.0,5.0]" $ run (Var "(+2.0)" `App` Var "map" `App` (ValDouble 3.0, ValDouble 4.0, ValDouble 5.0)) `eqBool` (Ok $ TLs [TF 3.0, TF 4.0, TF 5.0]),
      
      testCase "test for filter function"$
      do 
        assertBool "filter (>1) for empty list should return empty list" $ run (Var "(>1)" `App` Var "filter" `App` Nil)  `eqBool` (Ok $ TLs [])
        assertBool "for map (>1), the list [1,2,3] should return [3,4,5]" $ run (Var "(>1)" `App` Var "filter" `App` (ValInt 1, ValInt 2, ValInt 3)) `eqBool` (Ok $ TLs [TI 4, TI 5])
        assertBool "for map (>1.0), the list [1.0,2.0,3.0] should return [3.0,4.0,5.0]" $ run (Var "(>1.0)" `App` Var "filter" `App` (ValDouble 3.0, ValDouble 4.0, ValDouble 5.0)) `eqBool` (Ok $ TLs [TF 4.0, TF 5.0]),
      
      testCase "test for ord function"$
      do 
        assertBool "ord for a should return 97" $ run(Var "ord" `App` Var"a") `eqBool` (Ok $ I 97),

      testCase "test for chr function"$
      do 
        assertBool "chr for 97 should return a" $ run(Var "chr" `App` Var"97") `eqBool` (Ok $ C 'a'),

      testCase "test for int function"$
      do 
        assertBool "int for 4.5 should return 4" $ run(Var "int" `App` Var"4.5") `eqBool` (Ok $ I 4),
      
       testCase "test for float function"$
      do 
        assertBool "float for 4 should return 4.0" $ run(Var "float" `App` Var"4") `eqBool` (Ok $ F 4.0)
      

      
    ]




{-
   elem                       -- only for types with equality; for instance: (elem (\y -> 0) [(\x -> x*0)] ) should return an error
    map
    filter
    foldr   (optional -- but give it a shot!)
    ord     (char -> integer)
    chr     (integer -> char)
    float   (integer -> float)
    int     (float -> integer with truncation)


-}
