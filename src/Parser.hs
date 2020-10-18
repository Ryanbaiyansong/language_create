module Parser where

import Ast
import ParserMonad
import LangMonad
import Eval

-- | parser for the language
keywords = ["if","then","else", "let", "in", "true","false"]

{- should layer like this
1. not calls atoms
2. index calls consparse
3. exponential calls notparse
4. mult/div/mod calls expo
5. add/sub calls mult/div/mod
6. cons calls add/sub
7. concatparse = concat <|> index
8. eq/neq/lt/lte/gt/gte call concat

parser = sepParse

-}

vars :: Parser Ast
vars = do s <- token $ varParser
          if s `elem` keywords
          then failParse
          else return $ Var s

ints :: Parser Ast
ints = do s <- token $ intParser
          return $ ValInt s

bools :: Parser Ast
bools = do s <- token $ varParser
           case s of
             "true" -> return $ ValBool True
             "false" -> return $ ValBool False
             _ -> failParse
floats :: Parser Ast
floats = do s <- token $ floatParser
            return $ ValDouble s
            
chars :: Parser Ast
chars = do s <- token $ charParser
           if s `elem` keywords
           then failParse
           else return $ Var s

lists :: Parser Ast
lists = do token $ literal "["
           s <- token $ listExpr <||> (literal "]")
           case s of 
             Left s' -> return s'
             Right _ -> return Nil
          
listExpr :: Parser Ast
listExpr = do x <- parser
              token $ literal ","
              xs <- listExpr <||> (parser +++ (token $ literal "]"))
              case xs of 
                Left xs' -> return $ Cons x xs'
                Right (xs'', _) -> return $ Cons x (Cons xs'' Nil)


nil :: Parser Ast
nil = 
  do s <- token $ literal "[]"
     case s of 
          "[]" -> return Nil
          _ -> failParse

letExpr :: Parser Ast
letExpr = lambdaExpr <|> apps <|> consterm <|> term

trs::Parser Ast
trs = do token $ literal ","
         x <- varParser
         token $ literal "="
         mid <- letExpr
         trst <- trs <||>(token $ literal "in")
         case trst of
            Left x' -> return $ Let x mid x'
            Right _ -> do re <- letExpr
                          return $ Let x mid re

letParser :: Parser Ast
letParser =
 do token $ literal "let"
    v <- varParser
    token $ literal "="
    bod <- letExpr
    res <- trs <||> (token $ literal "in")
    case res of
      Left m -> return $ Let v bod m
      Right _ -> do re <- letExpr
                    return $ Let v bod re


-- *LangParser> parse letParser "let x=3 in x+x"
-- Just (let x = 3 in x + x,"")

parens :: Parser Ast
parens = do token $ literal "("
            res <- parser
            token $ literal ")"
            return res
{-
lists :: Parser Ast
lists = do token $ literal "["
           s <- token $ listExpr <||> (literal "]")
           case s of 
             Left s' -> return s'
             Right _ -> return Nil
          
listExpr :: Parser Ast
listExpr = do x <- parser
              token $ literal ","
              xs <- listExpr <||> (parser +++ (token $ literal "]"))
              case xs of 
                Left xs' -> return $ Cons x xs'
                Right (xs'', _) -> return $ Cons x (Cons xs'' Nil)
-}
lambdaExpr :: Parser Ast
lambdaExpr = apps <|> consterm <|>term
{-
lambdaParser :: Parser Ast
lambdaParser = do token $ literal "\\"
                  x <- varParser
                  token $ literal "->"
                  expr <- lambdaExpr
                  return $ Lam x expr
-}

subs::Parser Ast
subs = do s <- literal " "
          x <- varParser
          res <- subs <||> (token $ literal "->" +++ lambdaExpr)
          case res of
            Left x' -> do rest <- subs
                          return $ Lam x x'
            Right (_,bod) -> return $ Lam x bod

lambdaParser :: Parser Ast
lambdaParser = do token $ literal "\\"
                  x <- varParser
                  rest <- subs <||> (token $ literal "->")
                  case rest of
                    Left bod -> return $ Lam x bod
                    Right _ -> do body <- lambdaExpr
                                  return $ Lam x body

ifExpr :: Parser Ast
ifExpr = letExpr <|> lambdaExpr <|> apps <|> consterm <|> term

ifParser :: Parser Ast
ifParser =
 do token $ literal "if"
    cond <-  ifExpr
    token $ literal "then"
    l <-  ifExpr
    token $ literal "else"
    r <-  ifExpr
    return $ If cond l r


atoms = ints <|> chars <|> bools <|> floats <|> lists <|> nil <|> parens <|> ifParser <|> letParser <|> lambdaParser <|> vars

unaryMinusParser :: Parser Ast
unaryMinusParser = do token $ literal "-"
                      ls <- (unaryMinusParser <|> atoms)
                      return (Ne ls)

notExp :: Parser Ast
notExp = do token $ literal "!"
            res <- (notExp <|> atoms)
            return (Not res)

printParser :: Parser Ast
printParser =
  do token $ literal "print"
     ast <- parser
     let res = Print ast
     return res

factor = printParser <|> notExp <|> unaryMinusParser <|> atoms

lsIndexParser :: Parser Ast
lsIndexParser = do ls <- lists
                   token $ literal "!!"
                   v <- ints
                   return $ Li ls v

expExpr :: Parser Ast
expExpr = withInfix factor [("**", Ie),("^", Fe)]

multDivExpr :: Parser Ast
multDivExpr = withInfix (expExpr <|> factor<|>atoms) [("*", Mult), ("//", IntDiv),("/", DoubleDiv), ("%", Mod)]

plusterm :: Parser Ast
plusterm =  multDivExpr <|> expExpr<|> factor

addSubExpr :: Parser Ast
addSubExpr = withInfix plusterm [("+", Plus), ("-", Minus)]
-- *LangParser> parse addSubExpr "1+2+3+4"
-- Just (1 + 2 + 3 + 4,"")
-- *LangParser> parse addSubExpr "1-2-3-4"
-- Just (1 - 2 - 3 - 4,"")
term :: Parser Ast
term =  addSubExpr <|> multDivExpr <|> expExpr <|> lsIndexParser <|> factor

consterm :: Parser Ast
consterm = cons<|> term 

cons :: Parser Ast
cons = do l <- term
          consExpr l <|> return l

consExpr :: Ast -> Parser Ast
consExpr left = do token $ literal ":"
                   rest <- cons
                   let m = left `Cons` rest
                   (consExpr m) <|> return m

concatParser :: Parser Ast
concatParser = do first <- cons
                  token $ literal "++"
                  second <- cons
                  return $ Concat first second

expression :: Parser Ast
expression = cons <|> concatParser <|> term

-- contotalParser :: Parser Ast
-- contotalParser = concatParser <|> lsIndexParser

lessthanParser :: Parser Ast
lessthanParser = do first <- (ints <|> floats)
                    token $ literal "<"
                    second <- (ints <|> floats)
                    return $ Lt first second

greaterthanParser :: Parser Ast
greaterthanParser = do first <- (ints <|> floats)
                       token $ literal ">"
                       second <- (ints <|> floats)
                       return $ Gt first second

lessthaneqParser :: Parser Ast
lessthaneqParser = do first <- (ints <|> floats)
                      token $ literal "<="
                      second <- (ints <|> floats)
                      return $ Le first second

greaterthaneqParser :: Parser Ast
greaterthaneqParser = do first <- (ints <|> floats)
                         token $ literal ">="
                         second <- (ints <|> floats)
                         return $ Ge first second

eqParser :: Parser Ast
eqParser = do first <- expression
              token $ literal "=="
              second <- expression
              return $ Eqq first second

noteqParser :: Parser Ast
noteqParser = do first <- expression
                 token $ literal "/="
                 second <- expression
                 return $ NEq first second

compareParser :: Parser Ast
compareParser = lessthanParser <|> greaterthanParser <|> lessthaneqParser <|> greaterthaneqParser <|> eqParser <|> noteqParser <|> expression

andExpr :: Parser Ast
andExpr = withInfix compareParser [("&&", And)]

orExpr :: Parser Ast
orExpr = withInfix (andExpr <|> compareParser) [("||", Or)]

booleancompare :: Parser Ast
booleancompare = orExpr <|> andExpr <|> compareParser

-- *LangParser> parse orExpr "true || false && 7"
-- Just (true || false && 7,"")
-- *LangParser> parse orExpr "true || false || 7"
-- Just (true || false || 7,"")
-- *LangParser> parse orExpr "true"
-- Just (true,"")

apps :: Parser Ast
apps = withInfix booleancompare [("",App)] -- the tokens eat up all the spaces so we split on the empty string


appsterm :: Parser Ast
appsterm = apps <|> booleancompare

infixs :: Parser Ast
infixs = do l <- appsterm
            infixsExpr l <|> return l

infixsExpr :: Ast -> Parser Ast
infixsExpr left = do token $ literal "."
                     rest <- infixs
                     let m = left `Inf` rest
                     (infixsExpr m) <|> return m
appsandinfixs :: Parser Ast 
appsandinfixs = infixs <|> appsterm


sep :: Ast -> Parser Ast
sep left =
    do s <- (token $ literal ";")
       exp <- (appsandinfixs <|> apps <|> booleancompare)
       let res = left `Separator` exp
       (sep res) <|> return res

-- *LangParser> parse cons "1 : 4: true"
-- Just (1 : 4 : true,"")

parser :: Parser Ast
parser = do l<- infixs <|> appsterm <|> booleancompare
            sep l<|>return l





-- *LangParser> parse andExpr "false"
-- Just (false,"")
-- *LangParser> parse andExpr "false && false"
-- Just (false && false,"")





-- *LangParser> parse atoms "111"
-- Just (111,"")
-- *LangParser> parse atoms "  true"
-- Just (true,"")



-- *LangParser> parse parser "(true)"
-- Just (true,"")
-- *LangParser> parse parser "let x = (if true and false then 3 else elsee) in x + x"
-- Just (let x = if true and false then 3 else elsee in x + x,"")





-- for repl testing
{-data LangOut = ParseError | RuntimeError String | Result Val deriving Show

exec :: String -> LangOut
exec s = case (parse parser) s of
  Just (ast,"") -> case run ast of
                     (Ok v,_) -> Result v
                     (Error e,_) -> RuntimeError e
  _  -> ParseError-}