module Main where

import Language

main :: IO ()
main = putStr $ unlines [show vs ++ " -> " ++ show (interpret peg time_env)
                        | (vs, time_env) <- iterations labels]
  where
    (labels, peg) = figure_3_c
    
    iterations [] = [([], emptyTimeEnv)]
    iterations (l:ls) = do
        v <- [0..10]
        fmap ((v:) `pair` insertTimeEnv l v) (iterations ls)
    
    pair f g (a, b) = (f a, g b)

figure_2_a :: ([Label], PEG Int)
figure_2_a = ([l], n1)
  where
    n1 = Lift2 (*) n2 (Const 5)
    n2 = Theta l (Const 0) n3
    n3 = Phi delta n4 n5
    n4 = Lift2 (+) (Const 3) n5
    n5 = Lift2 (+) (Const 1) n2
    
    l = 1
    delta = Lift2 (<=) n2 (Const 10)

figure_3_a :: ([Label], PEG Int)
figure_3_a = ([l], n1)
  where
    n1 = Eval l n2 n4
    n2 = Theta l (Const 0) n3
    n3 = Lift2 (+) (Const 2) n2
    n4 = Pass l n5
    n5 = Lift2 (>=) n2 (Const 29)
    
    l = 1

figure_3_b :: ([Label], PEG Int)
figure_3_b = ([l1, l2], n1)
  where
    n1 = Theta l2 n2 n3
    n2 = Theta l1 (Const 0) n4
    n3 = Lift2 (+) (Const 1) n1
    n4 = Eval l2 n1 n5
    n5 = Pass l2 n6
    n6 = Lift2 (>=) n7 (Const (10 :: Int))
    n7 = Theta l2 (Const 0) n8
    n8 = Lift2 (+) (Const 1) n7
    
    l1 = 1
    l2 = 2

figure_3_c :: ([Label], PEG Int)
figure_3_c = ([l1, l2], n1)
  where
    n1 = Lift2 (+) n2 n5
    
    n2 = Lift2 (*) (Const 10) n3
    n3 = Theta l1 (Const 0) n4
    n4 = Lift2 (+) (Const 1) n3
    
    n5 = Theta l2 (Const 0) n6
    n6 = Lift2 (+) (Const 1) n5
    
    l1 = 1
    l2 = 2