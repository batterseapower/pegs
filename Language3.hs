module Language3 where

import Data.Maybe
import qualified Data.IntMap as IM


type LambdaLabel = Int

type CallSiteLabel = Int

type Instance = [CallSiteLabel]
type InstanceEnv = IM.IntMap Instance

emptyInstanceEnv :: InstanceEnv
emptyInstanceEnv = IM.empty

insertInstanceEnv :: LambdaLabel -> Instance -> InstanceEnv -> InstanceEnv
insertInstanceEnv = IM.insert

data PEG a where
    Phi :: PEG Bool -> PEG a -> PEG a -> PEG a
    Theta :: LambdaLabel -> PEG a -> CallSiteLabel -> PEG a -> PEG a
    Eval :: LambdaLabel -> PEG a -> PEG Instance -> PEG a
    Pass :: LambdaLabel -> CallSiteLabel -> PEG Instance
    Lam :: LambdaLabel -> (a -> PEG b) -> PEG (a -> b)
    App :: PEG (a -> b) -> PEG a -> PEG b
    Const :: a -> PEG a
    Lift :: (a -> b) -> PEG a -> PEG b
    Lift2 :: (a -> b -> c) -> PEG a -> PEG b -> PEG c


interpret :: PEG a -> InstanceEnv -> a
-- Phi nodes are similar to expressions using the ternary operator.
-- They return the value from their left or right branch depending
-- on the value of the condition.
interpret (Phi conds ts fs) inst_env
  | interpret conds inst_env = interpret ts inst_env
  | otherwise                = interpret fs inst_env
-- We use Theta nodes to represent values that vary depending on the
-- path taken through a recursive call. Intuitively, the left child of
-- a theta node computes the initial value, whereas the right child
-- is able to compute the value at the current iteration in terms of
-- the value at the previous iteration, IF the most recent call site
-- matches what it demands.
interpret (Theta l bases for_cs recurses) inst_env
  | (cs:css) <- l_inst
  , cs == for_cs       = interpret recurses (IM.insert l css inst_env)
  | otherwise          = interpret bases inst_env
  where l_inst = fromJust $ IM.lookup l inst_env
-- I'm not QUITE sure what this means, but it seems to be necessary in
-- the semantics I have devised
interpret (Pass l cs) inst_env
  = cs : (fromJust $ IM.lookup l inst_env)
-- Use HOAS to build PEGs representing values with lambdas
interpret (Lam l bodies) inst_env
  = \arg -> interpret (bodies arg) (IM.insert l [] inst_env)
-- Use HOAS to build PEGs representing the result of application
interpret (App fs args) inst_env
  = interpret fs inst_env (interpret args inst_env)
-- Produces the element of the left hand sequence at the instance given
-- by the right hand sequence
interpret (Eval l vals insts) inst_env
  = case interpret insts inst_env of
      inst -> monotonize l vals inst_env inst
-- These three boring cases just let us lift Haskellish stuff into PEGs
interpret (Const x) _ = x
interpret (Lift f args) inst_env
  = f (interpret args inst_env)
interpret (Lift2 f lefts rights) inst_env
  = f (interpret lefts inst_env) (interpret rights inst_env)


-- This curious function ensures that recursive calls to the function higher
-- up the stack have completed before we commit to the answer returned at
-- the given instance of the recursion
monotonize :: LambdaLabel -> PEG a -> InstanceEnv -> Instance -> a
monotonize l vals inst_env insts = go (reverse insts) []
  where
    go []           done = interpret vals (IM.insert l done inst_env)
    go (next:nexts) done = case interpret vals (IM.insert l (next:done) inst_env) of
                               _ -> go nexts (next:done)