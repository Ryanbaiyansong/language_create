module ParserTest where

import Test.Tasty (testGroup)
import Test.Tasty.HUnit (assertEqual, assertBool, testCase)
--import Test.Tasty.QuickCheck

import Ast
import Parser(parser)
import ParserMonad(parse)

--copy from week10 && to be modified
instance Arbitrary Ast where
    arbitrary = sized arbitrarySizedAst

arbitrarySizedAst ::  Int -> Gen Ast
arbitrarySizedAst m | m < 1 = do i <- arbitrary
                                 b <- arbitrary
                                 c <- arbitrary
                                 d <- arbitrary
                                 node <- elements [ValInt i, ValBool b, ValChar c, ValDouble d, Nil]
                                 return $ node
arbitrarySizedAst m | otherwise = do l <- arbitrarySizedAst (m `div` 2)
                                     r <- arbitrarySizedAst (m `div` 2)
                                     str <- elements ["x","y","z"]
                                     ifast <- arbitrarySizedIf m
                                     node <- elements [And l r, Or l r, Not l,
                                                       Plus l r, Minus l r, Mult l r,
                                                       IntDiv l r, DoubleDiv l r, Mod l r,
                                                       Fe l r, Ie l r,
                                                       Separator l r,
                                                       Eq l r, NEq l r,
                                                       LT l r, LE l r, GT l r, GE l r,
                                                       Cons l r, LI l r, Concat l r,
                                                       ifast,
                                                       Let str l r,
                                                       Lam str l,
                                                       App l r,
                                                       Var str
                                                      ]

                                     return node

                                     {-
                                        -          unary minus
                                        \ and ->   Lambda abstraction constructors
                                        [ and ]    List constructors, "," as separator
                                        ' and '    Char constructors
                                        " and "    String constructors
                                        --    Start of comment line (ignore everything until the next newline)
                                        {-    Start of multi-line comment
                                        -}    End of multi-line comment
                                     -}

-- it would be better if every branch were built like this so the balance would be maintained
arbitrarySizedIf ::  Int -> Gen Ast
arbitrarySizedIf m = do b <- arbitrarySizedAst (m `div` 3)
                        t <- arbitrarySizedAst (m `div` 3)
                        e <- arbitrarySizedAst (m `div` 3)
                        return $ If b t e

-- provide tests that show your parser works

partests = testGroup "ParserTest"
  [
  --error "no tests yet!"
  testProperty "parse should return the same AST when fully parenthisized" $ ((\ x -> (Ok x , []) == (parse parser $ showFullyParen x)) :: Ast -> Bool),
  testProperty "parse should return the same AST when pretty printed" $ ((\ x -> (Ok x , []) == (parse parser $ showPretty x 0)) :: Ast -> Bool)
  ]
