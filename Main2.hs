module Main where

import Language2

main :: IO ()
main = putStr $ unlines [show insts ++ " -> " ++ show (interpret peg inst_env)
                        | (insts, inst_env) <- iterations labels]
  where
    (labels, peg) = fib
    
    iterations [] = [([], emptyInstanceEnv)]
    iterations ((l, css):ls) = do
        cs <- css
        inst <- map (flip replicate cs) [0..10]
        fmap ((inst:) `pair` (insertInstanceEnv l inst)) (iterations ls)
    
    pair f g (a, b) = (f a, g b)

type ExamplePEG = ([(FunctionLabel, [CallSiteLabel])], PEG Int)

figure_2_a :: ExamplePEG
figure_2_a = ([(l, [cs])], n1)
  where
    n1 = Lift2 (*) n2 (Const 5)
    n2 = Theta l (Const 0) [n3]
    n3 = Phi delta n4 n5
    n4 = Lift2 (+) (Const 3) n5
    n5 = Lift2 (+) (Const 1) n2
    
    l = 1
    cs = 0
    delta = Lift2 (<=) n2 (Const 10)

fib :: ExamplePEG
fib = ([(l, [csl, csr])], n1)
  where
    n1 = Phi n2 (Const 1) n6
    n2 = Lift2 (<) n3 (Const 2)
    
    n3 = Theta l (Const 10) [n4, n5]
    n4 = Lift2 (-) n3 (Const 1)
    n5 = Lift2 (-) n3 (Const 2)
    
    n6 = Lift2 (+) n7 n8
    n7 = Eval l n1 (Pass l csl)
    n8 = Eval l n1 (Pass l csr)
    
    l = 1
    csl = 0
    csr = 1