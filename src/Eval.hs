module Eval where

import Data.Map (Map)
import Data.Char (ord, chr)
import qualified Data.Map as Map
import LangMonad
import Ast


-- the goal of the program is to return a value, what values are possible?
data Val = I Integer | F Double | B Bool 
          | C Char
          | Ls [Val]
          | Fun (Val -> (Unsafe Val, [String]))-- ...

instance Show Val where
  show (I i) = show i
  show (F f) = show f 
  show (B b) = show b
  show (C c) = show c
  show (Ls ls) = show ls
  show (Fun _) = "\\ x -> ?" -- no good way to show a function


{-
--(envType -> (Unsafe resType,[String]))
printThis :: x -> EnvUnsafeLog envType x
printThis x = EnvUnsafeLog $\e -> (x , [])
-}


-- | helper function that runs with the default environment (for example, the stdLib in week 10)
-- return either the error string or the value, along with everything that was printed
run :: Ast  -- ^ run this Ast
      -> (Unsafe Val, [String])  -- ^ (error message or result value, all the printings)
run a = runEnvUnsafelog(eval a) stdLib

-- Here is the abstract syntax tree for our language

stdLib = Map.fromList
  [("tail", Fun $ \ v -> case v of 
                             Ls (l:ls) -> ((Ok $ Ls ls), [])
                             _         -> (Error "can only call tail on a non empty list",["can only call tail on a non empty list"])),
   ("head", Fun $ \ v -> case v of 
                             Ls (l:ls) -> (Ok $ l,[])
                             _        -> (Error "can only call head on a non empty list",[])),
   ("len", Fun $ \ v -> case v of 
                             Ls a -> (Ok $ I (fromIntegral (length a)),[])
                             _ -> (Error "can only call len on a non empty list",[])),
   ("elem", Fun $ \ lst -> ((Ok (Fun (\element -> (case lst of
                                                    (Ls lst') -> (elem1 lst' element,[])
                                                    _ -> (Error "can only call elem on a non empty list",[] )
                                                    ))),[]))),
   
   ("filter", Fun $ \ lst -> ((Ok (Fun (\f -> (case lst of 
                                                  (Ls lst') -> case f of 
                                                                    (Fun f') -> (filter1 lst' f, [])
                                                                    _ -> (Error "first arg of filter must be a list", [])
                                                  _ -> (Error "Error", [])
                                                                    ))),[]))),
   ("map", Fun $ \ lst -> ((Ok ( Fun (\f -> (case lst of
                                                  (Ls lst') -> case f of 
                                                                    (Fun f') -> (map1 lst' f,[])
                                                                    _ -> (Error "first arg of map must be a list",[])
                                                  _ -> (Error "Error", [])
                                                                    ))),[]))),                            
   ("ord", Fun $ \ v -> case v of C c -> (Ok $ I $ (fromIntegral (fromEnum c)), [])
                                  _   -> (Error "ord Error",[])),
   ("chr", Fun $ \ v -> case v of I i -> (Ok $ C $ (toEnum (fromIntegral i)::Char), [])
                                  _   -> (Error "chr Error",[])),
   ("int", Fun $ \ v -> case v of F i -> (Ok $ I $ round i, [])
                                  _   -> (Error "int Error",[])),
   ("float", Fun $ \ v -> case v of I d -> (Ok $ F $ (fromIntegral d :: Double), [])
                                    _   -> (Error "float Error",[]))
  ]
  
                             


elem1 :: [Val] -> Val -> Unsafe Val 
elem1 [] _       = Ok (B False)
elem1 (x:xs) el = if (eqVal el x) then (Ok (B True)) else (elem1 xs el) 

eqVal :: Val -> Val -> Bool
eqVal (I a )(I b) = (a==b)
eqVal (F a )(F b) = (a==b)
eqVal (B a )(B b) = (a==b)
eqVal (C a )(C b) = (a==b)
eqVal (Ls (x:xs))(Ls (y:ys))= if eqVal x y then eqVal (Ls xs) (Ls ys) else False
eqVal _ _ = False


neqVal :: Val -> Val -> Bool
neqVal (I a )(I b) = (a/=b)
neqVal (F a )(F b) = (a/=b)
neqVal (B a )(B b) = (a/=b)
neqVal (C a )(C b) = (a/=b)
neqVal (Ls (x:xs))(Ls (y:ys))= if neqVal x y then True else neqVal (Ls xs) (Ls ys)
neqVal _ _ = True


map1 :: [Val] -> Val -> Unsafe Val
map1 [] _ = Ok (Ls []) 
map1 (x:xs) f = case (map1 xs f) of 
                      Ok (Ls mapped) -> case f of 
                                          Fun f' -> case (f' x) of 
                                                      (Ok y, _) -> Ok (Ls ([y] ++ mapped))
                                                      _         -> Error "fun didn't return an unsafe?" 
                                          _ -> (Error "second arg of map' must be a function")
                      _              -> (Error "first arg of map1 must be a list")



filter1 :: [Val] -> Val -> Unsafe Val
filter1 [] _   = Ok (Ls [])
filter1 (x:xs) f = case (filter1 xs f) of
                       Ok (Ls mapped) -> case f of 
                                          Fun f' -> case (f' x) of 
                                                      (Ok (B True), _) -> Ok (Ls ([x] ++ mapped))
                                                      (Ok (B False), _) -> Ok (Ls mapped)
                                                      _         -> Error "fun didn't return an unsafe?" 
                                          _ -> (Error "second arg of map' must be a function")
                       _              -> (Error "first arg of map1 must be a list")


type Env = Map String Val


-- some helper function, you may find helpful
valOf :: String -> EnvUnsafeLog Env Val
valOf var = EnvUnsafeLog $ \e -> case (Map.lookup var e) of 
                                Nothing -> (Error "Error", ["Error"]) 
                                Just x -> (Ok x,[])

-- add a val into the environment
withVal :: String -> Val -> EnvUnsafeLog Env a -> EnvUnsafeLog Env a
withVal var v comp = do env <- getEnv
                        case runEnvUnsafelog comp (Map.insert var v env) of
                          (Error m, _ )-> err "Error"
                          (Ok i, _ ) -> return i


-- helper functions that take care of type issues (use a "Error" when things have the wron type
evalInt :: Ast -> EnvUnsafeLog Env Integer
evalInt a = 
  do res <- eval a
     case res of
        I i -> return i
        _ -> err "Error"  

evalBool :: Ast -> EnvUnsafeLog Env Bool
evalBool a = 
  do res <- eval a
     case res of 
       B b -> return b
       _ -> err "Error"

evalDouble :: Ast -> EnvUnsafeLog Env Double 
evalDouble a = 
  do res <- eval a
     case res of 
       F b -> return b
       _ -> err "Error"

evalChar :: Ast -> EnvUnsafeLog Env Char 
evalChar a = 
  do res <- eval a 
     case res of 
       C c -> return c 
       _ -> err "Error"

evalFun :: Ast -> EnvUnsafeLog Env (Val -> (Unsafe Val,[String]))
evalFun a = 
  do res <- eval a
     case res of 
       Fun f -> return f
       _ -> err "Error"




eval :: Ast -> EnvUnsafeLog Env Val
eval (ValBool b) = return $ B b
eval (ValChar c) = return $ C c
eval (And a b) =
  do resL <- evalBool a
     resR <- evalBool b
     case resL of
       False -> return $ B resL
       True -> return $ B $ resR

eval (Or a b) = 
  do resL <- evalBool a
     resR <- evalBool b
     case resL of
       True -> return $ B resL
       False -> return $ B resR

eval (Not a) = 
  do res <- evalBool a
     case res of 
       True -> return $ B $ False
       False -> return $ B $ True

eval (ValInt i) = return $ I i
eval (ValDouble d) = return $ F d 

eval (Plus l r ) = 
  do resL <- eval l
     resR <- eval r
     case (resL, resR) of
         (I a, I b) -> return $ I $ a + b
         (F a, F b) -> return $ F $ a + b
         _ -> err "Error"


eval (Minus l r) =
  do resL <- eval l
     resR <- eval r
     case (resL, resR) of
         (I a, I b) -> return $ I $ a - b
         (F a, F b) -> return $ F $ a - b
         _ -> err "Error"

eval (Mult l r) = 
  do resL <- evalInt l
     resR <- evalInt r
     return $ I $ resL * resR

eval (IntDiv l r) = 
  do resL <- evalInt l
     resR <- evalInt r
     case resR of 
       0 -> err "Error"
       _ -> return $ I $ resL `div` resR

eval (DoubleDiv l r) = 
  do resL <- evalDouble l
     resR <- evalDouble r
     case resR of 
       0.0 -> err "Error"
       _ -> return $ F $ resL / resR
        
eval Nil = return $ Ls $ []

eval (Cons l r) = 
  do resL <- eval l
     resR <- eval r
     case resR of 
       Ls r' -> return $ Ls $ [resL] ++ r'
       _  -> err "Error"

eval (If a b c) = 
  do res1 <- evalBool a 
     res2 <- eval b
     res3 <- eval c
     case res1 of 
       True -> return res2
       False -> return res3

eval (Let a b c) = 
  do res <- eval c
     (withVal a res (eval b))

eval (Var s) = valOf s
  
eval (Lam x bod) = 
  do env <- getEnv
     return $ Fun $ \v -> runEnvUnsafelog (eval bod) (Map.insert x v env)

eval (App x y) = 
  do resL <- eval x
     resR <- eval y
     case resL of
       Fun func -> case (func resR) of
                     (Ok a, [])-> return a
                     _ -> err "Error"
       _ -> err "Error"
eval (x `Separator` y) =
  do resL <- eval x
     resR <- eval y
     return resR

eval (Eqq x y) =
  do resL <- eval x
     resR <- eval y
     case (resL, resR) of 
       (I a, I b) -> return $ B (a==b)
       (F a, F b) -> return $ B (a==b)
       (B a, B b) -> return $ B (a==b)
       (C a, C b) -> return $ B (a==b)
       (Ls a, Ls b) -> return $ B $ (eqVal (Ls a) (Ls b))
       _ -> return $ B False




eval (NEq x y) = 
  do resL <- eval x
     resR <- eval y
     case (resL, resR) of 
       (I a, I b) -> return $ B (a/=b)
       (F a, F b) -> return $ B (a/=b)
       (B a, B b) -> return $ B (a/=b)
       (C a, C b) -> return $ B (a/=b)
       (Ls a, Ls b) -> return $ B $ (neqVal (Ls a) (Ls b))
       _ -> return $ B False

eval (Lt x y)= 
  do resL <- eval x
     resR <- eval y
     case (resL, resR) of
       (I x', I y') -> return $ B $ x' < y'
       (F m', F n') -> return $ B $ m' < n'
       _ -> err "Error"

eval (Le x y)= 
  do resL <- eval x
     resR <- eval y
     case (resL, resR) of
       (I x', I y') -> return $ B $ x' <= y'
       (F m', F n') -> return $ B $ m' <= n'
       _ -> err "Error"

eval (Gt x y) = 
  do resL <- eval x
     resR <- eval y
     case (resL, resR) of
       (I x', I y') -> return $ B $ x' > y'
       (F m', F n') -> return $ B $ m' > n'
       _ -> err "Error"

eval (Ge x y) =
  do resL <- eval x
     resR <- eval y
     case (resL, resR) of
       (I x', I y') -> return $ B $ x' >= y'
       (F m', F n') -> return $ B $ m' >= n'
       _ -> err "Error"

eval (Concat x y) = 
   do resL <- eval x
      resR <- eval y
      case (resL, resR) of 
        (Ls x', Ls y') -> return $ Ls (x' ++ y')
        _ -> err "Error"

eval (Fe x y) = 
    do resL <- eval x
       resR <- eval y
       case (resL, resR) of
         (F x', F y') -> return $ F $ x' ** y'
         _ -> err "Error"

eval (Ie x y) = 
    do resL <- eval x
       resR <- eval y
       case (resL, resR) of
         (I x', I y') -> return $ I $ x' ^ y'
         _ -> err "Error"

eval (Mod x y) = 
    do resL <- eval x
       resR <- eval y
       case (resL, resR) of
         (I x', I y') -> return $ I $ x' `mod` y' 
         _ -> err "Error"

eval (Li x y) = 
    do resL <- eval x
       resR <- eval y
       case (resL, resR) of
         (Ls x', I y') -> return $ (x' !! (fromIntegral y'))
         _ -> err "Error"

eval (Ne x) =
    do resL <- eval x
       case resL of 
         I x' -> return $ I $ -x'
         F y -> return $ F $ -y
         _ -> err "Error"


eval(Print x) = do x' <- eval x 
                   EnvUnsafeLog (\env -> (Ok x',[show x']))
               
eval (Inf l r) = 
      do resL <- eval l
         resR <- eval r
         case (resL, resR) of
              (Fun a, Fun b) -> return $ Fun $ a.(return $ Fun $ b)
              _ -> err "Error"