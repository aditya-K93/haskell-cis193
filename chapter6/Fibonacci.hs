{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Fibonacci where

fib :: Integer -> Integer
fib n | n == 0    = 0
      | n == 1    = 1
      | otherwise = fib (n - 1) + fib (n - 2)

fib1 :: [Integer]
fib1 = map fib [0 ..]

fib2 :: [Integer]
fib2 = fibFast 0 1 where fibFast y z = y : fibFast z (y + z)

data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons y ys) = y : streamToList ys

instance Show a => Show (Stream a) where
    show = show . take 20 . streamToList

streamRepeat :: a -> Stream a
streamRepeat y = Cons y (streamRepeat y)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons y ys) = Cons (f y) (streamMap f ys)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f y = Cons y (streamFromSeed f (f y))

nats :: Stream Integer
nats = streamFromSeed (+ 1) 0

interLeaveStreams :: Stream a -> Stream a -> Stream a
interLeaveStreams (Cons y ys) zs = Cons y (interLeaveStreams zs ys)

-- alternate (Stream0  (alternate (Stream1 (alternate Stream2 .. ) ) ) )
ruler :: Stream Integer
ruler = runRuler 0
    where runRuler y = interLeaveStreams (streamRepeat y) (runRuler $ y + 1)

-- coeffiecient values 0,1,0,0, from x = 0x + 1x + 0x^2 .. 
x :: Stream Integer
x = Cons 0 (Cons 1 (streamRepeat 0))

-- fromInterger similar to x with n as first Coeff rest all 0
instance Num (Stream Integer) where
    fromInteger y = Cons y (streamRepeat 0)
    negate (Cons y ys) = Cons (-y) (negate ys)
    (+) (Cons y ys) (Cons z zs) = Cons (y + z) (ys + zs)
    (*) (Cons y ys) s@(Cons z zs) =
        Cons (y * z) (streamMap (y *) zs + (ys * s))

instance Fractional (Stream Integer) where
    (/) (Cons y ys) (Cons z zs) = q
        where q = Cons (y `div` z) (streamMap (`div` z) (ys - q * zs))

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x ^ (2 :: Integer))

data Matrix = Matrix Integer Integer Integer Integer deriving Show

instance Num Matrix where
    (*) (Matrix a11 a12 a21 a22) (Matrix b11 b12 b21 b22) = Matrix
        (a11 * b11 + a12 * b21)
        (a11 * b12 + a12 * b22)
        (a21 * b11 + a22 * b21)
        (a21 * b12 + a22 * b22)

fib4 :: Integer -> Integer
fib4 a | a == 0    = 0
       | otherwise = fibFromMatrix $ m ^ a
  where
    fibFromMatrix (Matrix _ n _ _) = n
    m = Matrix 1 1 1 0

