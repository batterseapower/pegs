module Language where

import Data.Maybe
import qualified Data.IntMap as IM


type Label = Int

type Time = Int
type TimeEnv = IM.IntMap Time

emptyTimeEnv :: TimeEnv
emptyTimeEnv = IM.empty

insertTimeEnv :: Label -> Time -> TimeEnv -> TimeEnv
insertTimeEnv = IM.insert

data PEG a where
    Phi :: PEG Bool -> PEG a -> PEG a -> PEG a
    Theta :: Label -> PEG a -> PEG a -> PEG a
    Eval :: Label -> PEG a -> PEG Time -> PEG a
    Pass :: Label -> PEG Bool -> PEG Time
    Const :: a -> PEG a
    Lift :: (a -> b) -> PEG a -> PEG b
    Lift2 :: (a -> b -> c) -> PEG a -> PEG b -> PEG c


interpret :: PEG a -> TimeEnv -> a
-- Phi nodes are similar to expressions using the ternary operator.
-- They return the value from their left or right branch depending
-- on the value of the condition.
interpret (Phi conds ts fs) time_env
  | interpret conds time_env = interpret ts time_env
  | otherwise                = interpret fs time_env
-- We use Theta nodes to represent values that vary inside of a loop.
-- Intuitively, the left child of a theta node computes the initial
-- value, whereas the right child computes the value at the current
-- iteration in terms of the value at the previous iteration.
interpret (Theta l bases loops) time_env
  | l_time <= 0 = interpret bases time_env
  | otherwise   = interpret loops (IM.insert l (l_time - 1) time_env)
  where l_time = fromJust $ IM.lookup l time_env
-- Produces the element of the left hand sequence at the time given
-- by the right hand sequence.
interpret (Eval l vals idxs) time_env
  = case interpret idxs time_env of
        idx -> monotonize l vals time_env idx
-- Returns the time of the first element in the sequence of conditions
-- that is true. Used for determining when it is that a loop exits.
interpret (Pass l conds) time_env = go 0
  where
    go i | monotonize l conds time_env i = i
         | otherwise                     = go (i + 1)
-- These three boring cases just let us lift Haskellish stuff into PEGs
interpret (Const x) _ = x
interpret (Lift f args) time_env
  = f (interpret args time_env)
interpret (Lift2 f lefts rights) time_env
  = f (interpret lefts time_env) (interpret rights time_env)


monotonize :: Label -> PEG a -> TimeEnv -> Int -> a
monotonize l vals time_env at_time = go 0
  where
    go i = case interpret vals (IM.insert l i time_env) of
               val | i < at_time -> go (i + 1)
                   | otherwise   -> val