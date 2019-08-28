module Golf where

import qualified Data.Map                      as Map

skips :: [a] -> [[a]]
skips xs = map (listAtInterval xs ) [1.. length xs]
 where
  listAtInterval xs interval =
    map fst $ filter (\x -> (snd x `mod` interval) == 0) $ zip xs [1 ..]

localMaxima :: [Integer] -> [Integer]
localMaxima (x : y : z : zs) | x < y && y > z = y : localMaxima (y : z : zs)
                             | otherwise      = localMaxima (y : z : zs)
localMaxima _ = []

histogram :: [Integer] -> String
histogram xs = response ++ "==========\n0123456789\n"
 where
  response = concatMap
    (\i ->
      foldl (\s e -> if e >= i then s ++ "*" else s ++ " ") "" count ++ "\n"
    )
    $reverse
    [1 .. maximum count]
    where count = map (\n -> length $ filter (== n) xs) [0 .. 9]
