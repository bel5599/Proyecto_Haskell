module Ejemplo where

collatz :: Int -> [Int]
collatz 1 = [1]
collatz n   |even n  = n:collatz(div n 2)
            |otherwise = n:collatz(3*n+1)