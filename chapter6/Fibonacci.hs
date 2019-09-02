module Fibonacci where

fib :: Integer -> Integer
fib n | n == 0    = 0
      | n == 1    = 1
      | otherwise = fib (n - 1) + fib (n - 2)

fib1 :: [Integer]
fib1 = map fib [0 ..]

fib2 :: [Integer]
fib2 = fibFast 0 1 where fibFast x y = x : fibFast y (x + y)
