module Check where

import Data.Set (Set)
import qualified Data.Set as Set

import Ast

-- here you can preform static checks


-- | The data type for all the static check warning
-- Some example include:
--   * use of undefined variable
--   * defined but unused variable
--   * type errors
data WarningMsg =
    UndefinedVarUse String
     -- ^ This is the Warning for use of Undefined variable name
  -- ...
  deriving (Show,Ord,Eq)

-- | perform static checking on the Ast
-- the output a set of warning on that input Ast
check :: Ast -> Set WarningMsg
check (Var x)= Set.fromList([UndefinedVarUse x])
check (Lam str ast) = Set.delete (UndefinedVarUse str) (check ast)
check (App t1 t2) = check t1 `Set.union` check t2
check (Let str t1 t2) = Set.delete (UndefinedVarUse str) (check t1 `Set.union` check t2)
check (If bol thn els) = (check thn) `Set.union` (check els)
check (Separator l r) = check l `Set.union` check r
check (And l r) = check l `Set.union` check r
check (Or l r) = check l `Set.union` check r
check (Plus x y) = check x `Set.union` check y
check (Minus x y) = check x `Set.union` check y
check (Mult x y) = check x `Set.union` check y
check (IntDiv x y) = check x `Set.union` check y
check (DoubleDiv x y) = check x `Set.union` check y
check (Mod x y) = check x `Set.union` check y
check (Ie x y) = check x `Set.union` check y
check (Fe x y) = check x `Set.union` check y
check (Cons x y) = check x `Set.union` check y
check (Concat x y) = check x `Set.union` check y
check (Eqq x y) = check x `Set.union` check y
check (NEq x y) = check x `Set.union` check y
check (Lt x y) = check x `Set.union` check y
check (Le x y) = check x `Set.union` check y
check (Gt x y) = check x `Set.union` check y
check (Ge x y) = check x `Set.union` check y
check (Print x) = check x
check (Not x) = check x
check (Ne x) = check x
check (Li x y) = check x -- y is an Int
check (Inf x y) = check x `Set.union` check y
check _ = Set.empty     -- ints, floats, bools, strings, and chars do not contain vars