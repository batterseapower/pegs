module Main where

import Language3

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

figure_2_a :: ([(LambdaLabel, [CallSiteLabel])], PEG Int)
figure_2_a = ([(l, [cs])], n1)
  where
    n1 = Lift2 (*) n2 (Const 5)
    n2 = Theta l (Const 0) cs n3
    n3 = Phi delta n4 n5
    n4 = Lift2 (+) (Const 3) n5
    n5 = Lift2 (+) (Const 1) n2
    
    l = 1
    cs = 0
    delta = Lift2 (<=) n2 (Const 10)

fib :: ([(LambdaLabel, [CallSiteLabel])], PEG Int)
fib = ([], n1)
  where
    ([(l, _)], build_body) = fib_body
    
    n1 = App n2 n3
    n2 = Lam l (\n -> build_body (Const n))
    n3 = Const 10

fib_body :: ([(LambdaLabel, [CallSiteLabel])], PEG Int -> PEG Int)
fib_body = ([(l, [csl, csr])],
            \initial_n ->
              let n1 = Phi n2 (Const 1) n6
                  n2 = Lift2 (<) n3 (Const 2)
    
                  n3 = Theta l (Theta l initial_n csl n4) csr n5
                  n4 = Lift2 (-) n3 (Const 1)
                  n5 = Lift2 (-) n3 (Const 2)
    
                  n6 = Lift2 (+) n7 n8
                  n7 = Eval l n1 (Pass l csl)
                  n8 = Eval l n1 (Pass l csr)
              in n1)
  where
    l = 1
    csl = 0
    csr = 1