{-# OPTIONS_GHC -Wall#-}

module HOF where

import           Data.List

fun1 :: [Integer] -> Integer
fun1 = product . map (subtract 2) . filter even

fun2 :: Integer -> Integer
fun2 = sum . filter even . takeWhile (/= 1) . iterate
  (\n -> if odd n then 3 * n + 1 else n `div` 2)

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
 deriving (Show, Ord, Eq)


getTreeHeight :: Tree a -> Integer
getTreeHeight Leaf           = 0
getTreeHeight (Node n _ _ _) = n

foldTree :: (Ord a) => [a] -> Tree a
foldTree = foldr insertElem Leaf
 where
  insertElem x Leaf = Node 0 Leaf x Leaf
  insertElem x (Node _ left root right)
    | left > right = Node (getTreeHeight rightTree + 1) left root rightTree
    | otherwise    = Node (getTreeHeight leftTree + 1) leftTree root right
   where
    rightTree = insertElem x right
    leftTree  = insertElem x left


(!!!) :: Bool -> Bool -> Bool
(!!!) a b = (a || b) && not (a && b)

xor :: [Bool] -> Bool
xor = foldr (!!!) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x y -> f x : y) []

myfoldl :: (a -> b -> a) -> a -> [b] -> a
myfoldl f base = foldr (flip f) base . reverse

sieveSundaram :: Integer -> [Integer]
sieveSundaram n =
  map ((+ 1) . (* 2))
    . (\\) [1 .. n]
    $ map (\(i, j) -> i + j + 2 * i * j)
    . filter (\(i, j) -> i <= j && ((i + j + 2 * i * j) <= n))
    $ cartProd [1 .. n] [1 .. n]

cartProd :: [a] -> [a] -> [(a, a)]
cartProd xs ys = [ (x, y) | x <- xs, y <- ys ]
