module Ast where
import Data.Map (Map)
import qualified Data.Map as Map

import HelpShow

import LangMonad
-- | the abstract syntax tree for the language
data Ast  = ValBool Bool
         | And Ast Ast | Or Ast Ast | Not Ast

         | ValInt Integer | ValDouble Double 
         | Plus Ast Ast | Minus Ast Ast | Mult Ast Ast | IntDiv Ast Ast | DoubleDiv Ast Ast

         | Nil
         | Cons Ast Ast
          
         | ValChar Char 

         |Eqq Ast Ast | NEq Ast Ast | Gt Ast Ast | Ge Ast Ast | Lt Ast Ast | Le Ast Ast 
         |Fe Ast Ast | Ie Ast Ast | Concat Ast Ast | Li Ast Ast | Mod Ast Ast | Ne Ast 

         |Inf Ast Ast

         | If Ast Ast Ast
         | Let String Ast Ast

         | Var String 
         | Lam String Ast
         | App Ast Ast
         | Separator Ast Ast
         | Print Ast 
--           deriving (Eq,Show) -- helpful to use this during testing
         deriving Eq-- ...
--         deriving (Eq,Show)  -- helpful to use this during testing
--         deriving Eq 

instance Show Ast where
  -- display the ast in a readable way
  show ast = showPretty ast 0


  

-- | output the fully parenthesized statement
showFullyParen :: Ast -> String
showFullyParen (ValInt i) = "(" ++ show i ++ ")"
showFullyParen (ValDouble f) = "(" ++ show f ++ ")"
showFullyParen (ValChar c) = "(" ++ show c ++ ")"
showFullyParen (ValBool True) = "(" ++ "true" ++ ")"
showFullyParen (ValBool False) = "(" ++ "false" ++ ")"
showFullyParen (And l r) = "(" ++ (showFullyParen l) ++ " && " ++ (showFullyParen r) ++ ")"
showFullyParen (Or l r) = "(" ++ (showFullyParen l) ++ " || " ++ (showFullyParen r) ++ ")"
showFullyParen (Not a) = "(" ++ " ! " ++ (showFullyParen a) ++ ")"
showFullyParen (Plus l r) = "(" ++ (showFullyParen l) ++ " + " ++ (showFullyParen r) ++ ")"
showFullyParen (Minus l r) = "(" ++ (showFullyParen l) ++ " - " ++ (showFullyParen r) ++ ")"
showFullyParen (Mult l r) = "(" ++ (showFullyParen l) ++ " * " ++ (showFullyParen r) ++ ")"
showFullyParen (IntDiv l r) = "(" ++ (showFullyParen l) ++ " / " ++ (showFullyParen r) ++ ")"
showFullyParen (DoubleDiv l r) = "(" ++ (showFullyParen l) ++ " // " ++ (showFullyParen r) ++ ")"
showFullyParen (If b t e) = "(if " ++ (showFullyParen b) ++ " then " ++ (showFullyParen t) ++ " else " ++ (showFullyParen e) ++ ")"
showFullyParen (Let v a bod) = "(let " ++ v ++ " = " ++ (showFullyParen a) ++ " in " ++ (showFullyParen bod) ++ ")"
showFullyParen (Lam v bod) = "(\\ " ++ v ++ " -> " ++ (showFullyParen bod) ++ ")"
showFullyParen (App f a) = "( " ++ (showFullyParen f)  ++ " " ++ (showFullyParen a) ++ ")"
showFullyParen (Var s) = "( " ++ s ++ ")"
showFullyParen (Cons h t) = "(" ++ (showFullyParen h)  ++ " : " ++ (showFullyParen t) ++ ")"
showFullyParen Nil = "( [] )"
showFullyParen (Eqq l r) = "(" ++ (showFullyParen l) ++ " == " ++ (showFullyParen r) ++ ")"
showFullyParen (NEq l r) = "(" ++ (showFullyParen l) ++ " /= " ++ (showFullyParen r) ++ ")"
showFullyParen (Mod l r) = "(" ++ (showFullyParen l) ++ "%" ++ (showFullyParen r) ++ ")"
showFullyParen (Lt l r) = "(" ++ (showFullyParen l) ++ "<" ++ (showFullyParen r) ++ ")"
showFullyParen (Le l r) = "(" ++ (showFullyParen l) ++ "<=" ++ (showFullyParen r) ++ ")"
showFullyParen (Gt l r) = "(" ++ (showFullyParen l) ++ ">" ++ (showFullyParen r) ++ ")"
showFullyParen (Ge l r) = "(" ++ (showFullyParen l) ++ ">=" ++ (showFullyParen r) ++ ")"
showFullyParen (Fe l r) = "(" ++ (showFullyParen l) ++ "^" ++ (showFullyParen r) ++ ")"
showFullyParen (Ie l r) = "(" ++ (showFullyParen l) ++ "**" ++ (showFullyParen r) ++ ")"
showFullyParen (Separator l r) = "(" ++ (showFullyParen l) ++ ";" ++ (showFullyParen r) ++ ")"
showFullyParen (Li l r) = "(" ++ (showFullyParen l) ++ "!!" ++ (showFullyParen r) ++ ")"
showFullyParen (Concat l r) = "(" ++ (showFullyParen l)  ++ " ++ " ++ (showFullyParen r) ++ ")"
showFullyParen (Ne l) = "(" ++ " - " ++ (showFullyParen l) ++ ")"
showFullyParen (Print l) = "print(" ++ (showFullyParen l) ++ ")"
showFullyParen (Inf l r) = "(" ++ (showFullyParen l) ++ " . " ++ (showFullyParen r) ++")"

-- provide a nice show with minimal parentheses, for testing an documentation



--the bigger the number the more tight the biding
showPretty :: Ast -> Integer -> String
showPretty (ValInt i) _ =  if i < 0
                           then  "(" ++ show i ++ ")"
                           else show i
showPretty (ValDouble f) _ = if f < 0
                             then "(" ++ show f ++ ")"
                             else show f
showPretty (ValChar c) _ =  "c"
showPretty (ValBool True) _ =  "true"
showPretty (ValBool False)  _  = "false"
showPretty Nil _ = "[]"
showPretty (Var s) _ = s
showPretty (Lam v bod) i   = parenthesize 1 i  $ "\\ " ++ v ++ " -> "        ++ (showPretty bod 100)
showPretty (Let v a bod) i = parenthesize 1 i  $  "let " ++ v ++ " = "       ++ (showPretty a 1) ++ " in " ++ (showPretty bod 1)
showPretty (If b t e) i    = parenthesize 1 i  $  "if " ++ (showPretty b 1)  ++ " then " ++ (showPretty t 1) ++ " else " ++ (showPretty e 1)
showPretty (Not l ) i      = parenthesize 2 i $  " ! " ++ (showPretty l 2)
showPretty (Print l) i = parenthesize 2 i $ "print" ++ (showPretty l 2)
showPretty (Ne l) i = parenthesize 2 i $ " - " ++ (showPretty l 2)
showPretty (Li l r) i = parenthesize 3 i $ (showPretty l 3) ++ " !! " ++ (showPretty r 3)
showPretty (Ie l r) i = parenthesize 5 i $ (showPretty l 5) ++ " ** " ++ (showPretty r 4)
showPretty (Fe l r) i = parenthesize 5 i $ (showPretty l 5) ++ " ^ " ++ (showPretty r 4)
showPretty (Mult l r) i    = parenthesize 6 i $ (showPretty l 6) ++ " * "  ++ (showPretty r 7)
showPretty (IntDiv l r) i     = parenthesize 6 i $ (showPretty l 6) ++ " // "  ++ (showPretty r 7)
showPretty (DoubleDiv l r) i     = parenthesize 6 i $ (showPretty l 6) ++ " / "  ++ (showPretty r 7)
showPretty (Mod l r) i = parenthesize 6 i $ (showPretty l 6) ++ " % " ++ (showPretty r 7)
showPretty (Minus l r) i   = parenthesize 8 i $ (showPretty l 8) ++ " - "  ++ (showPretty r 9)
showPretty (Plus l r) i    = parenthesize 8 i $ (showPretty l 8) ++ " + "  ++ (showPretty r 9)
showPretty (Cons l r) i    = parenthesize 11 i  $ (showPretty l 11)  ++ " : "  ++ (showPretty r 10)
showPretty (Concat l r) i = parenthesize  11 i $ (showPretty l 11) ++ " ++ " ++ (showPretty r 10)
showPretty (Eqq l r) i      = parenthesize 12 i  $ (showPretty l 12) ++ " == " ++ (showPretty r 13)
showPretty (NEq l r) i     = parenthesize 12 i  $ (showPretty l 12) ++ " /= " ++ (showPretty r 13)
showPretty (Lt l r) i = parenthesize 12 i $ (showPretty l 12) ++ " < " ++ (showPretty r 13)
showPretty (Le l r) i = parenthesize 12 i $ (showPretty l 12) ++ " <= " ++ (showPretty r 13)
showPretty (Gt l r) i = parenthesize 12 i $ (showPretty l 12) ++ " > " ++ (showPretty r 13)
showPretty (Ge l r) i = parenthesize 12 i $ (showPretty l 12) ++ " >=" ++ (showPretty r 13)
showPretty (And l r) i     = parenthesize 14 i  $ (showPretty l 14)  ++ " && " ++ (showPretty r 15)
showPretty (Or l r) i      = parenthesize 16 i  $ (showPretty l 16)  ++ " || " ++ (showPretty r 17)
showPretty (App l r) i     = parenthesize 18 i  $ (showPretty l 18)  ++ " "    ++ (showPretty r 19)
showPretty (Inf l r ) i     = parenthesize 18 i  $ (showPretty l 19)  ++ " "    ++ (showPretty r 18)
showPretty (Separator l r) i = parenthesize 21 i $ (showPretty l 21) ++ " ; " ++ (showPretty r 22)





