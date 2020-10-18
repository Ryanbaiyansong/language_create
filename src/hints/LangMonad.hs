-- This file is just for hints.
-- Don't edit this file unless you are comfortable resolving merge conflicts, since we may add hints in the future
-- Instead copy and paste what you want into your homework

module EnvUnsafeLog where

import Control.Monad(ap)

--This monad should form the plumbing for the evaluation function
-- This is a very rough outline



-- feel free to rename LangMonad, add parameters, anything you want!
data Unsafe a = Error String | Ok a deriving (Show,Eq)
data EnvUnsafeLog envType resType = EnvUnsafeLog(envType -> (Unsafe resType,[String]))

-- function that just runs the contents of LangMonad
runEnvUnsafelog :: (EnvUnsafeLog envType resType) -> envType -> (Unsafe resType,[String])
runEnvUnsafelog (EnvUnsafeLog eu) e = eu e


err :: String -> EnvUnsafeLog envType resType
err s = EnvUnsafeLog $ \ _ -> (Error s, [])

getEnv :: EnvUnsafeLog envType resType
getEnv = EnvUnsafeLog $ \ e -> (Ok e, [])

instance Functor (EnvUnsafeLog e) where
  fmap f (EnvUnsafeLog eu) = EnvUnsafeLog $ \e -> case (runEnvUnsafelog (EnvUnsafeLog eu) e) of
  	                                                 (Error m, log) -> (Error m, log)
  	                                                 (Ok val, log') -> (Ok (f val), log')

  
--ignore this for now
instance Applicative (EnvUnsafeLog e) where
  pure = return
  (<*>) = ap
  
instance Monad (EnvUnsafeLog e) where

  return a = EnvUnsafeLog $ \e -> (Ok a, []) 
  
  (EnvUnsafeLog eu) >>= f = EnvUnsafeLog $ \e ->
  								case (runEnvUnsafeLog (EnvUnsafeLog eu) e) of
  									(Error m, log) -> (Error m, log)
  									(Ok a, log') -> let (EnvUnsafeLog h) = f x
                                    in case (h e) of
                                          (res, log'') -> (res, log'' ++ log')



-- You could put some monad helper functions here