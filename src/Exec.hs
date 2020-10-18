module Exec where

import Data.Set (Set)
import qualified Data.Set as Set

import Ast
import Eval
import Parser
import Check
import LangMonad
import ParserMonad


data LangOut = 
    ParseError -- ^ retuned when the string could not be parsed
  | RuntimeError String [String]
  -- ^ retuned when there is a runtime error
  -- first String is the error message
  -- this list of Strings is what is printed before the error was encountered 
  | Ok Val [String]
  -- ^ retuned when the program runs successfully and return a value
  -- The Val is the evaluation result of the program
  -- The list of String is what gets printed while running the program

{-
-- | execute the program as a string and get the result
exec :: String -> LangOut
exec s = case parse parser s of
    Left _ -> ParseError
    Right (ast, []) ->
      let (result, prints) = run ast
      in
        case result of
          Error err -> RuntimeError err prints
          Ok val -> Ok val prints
    Right (_, _) -> error "parse incorrect"

-- | perform static checking on the program string, may be empty if there is a parse error
warn :: String -> (Set WarningMsg) 
warn s = case parse parser s of
         Left _ -> Set.empty
         Right (ast, []) -> check ast
         Right (_, _) -> error "parse incorrect"
-}
exec :: String -> LangOut
exec s = case (parse parser) s of
  Just (ast, "") -> case run ast of
                      (LangMonad.Ok v,[]) -> (Exec.Ok v [])
                      (Error e,[]) -> (RuntimeError e ["RuntimeError"])
  _ -> ParseError

-- | perform static checking on the program string, may be empty if there is a parse error
warn :: String -> (Set WarningMsg)
warn s = case (parse parser) s of
  Just (ast, "") -> check ast
  _ -> Set.empty

{-
-- for repl testing
data LangOut = ParseError | RuntimeError String | Result Val deriving Show

exec :: String -> LangOut
exec s = case (parse parser) s of
  Just (ast,"") -> case run ast of
                     Ok v -> Result v
                     Error e -> RuntimeError e
  _  -> ParseError
-}