module TowersHanoi where

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi 1 a b _ = move a b
hanoi n a b c = hanoi (n - 1) a c b ++ hanoi 1 a b c ++ hanoi (n - 1) c b a

move :: Peg -> Peg -> [Move]
move a b = [(a, b)]

toDigitsReverse :: Integer -> [Integer]
toDigitsReverse n | n == 0    = []
                  | n < 0     = []
                  | otherwise = n `mod` 10 : toDigitsReverse (n `div` 10)

toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsReverse

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther n =
    reverse $ map (\(x, y) -> if even x then 2 * y else y) $ zip
        [1 .. length n]
        (reverse n)

sumDigits :: [Integer] -> Integer
sumDigits = sum . concatMap toDigits

validate :: Integer -> Bool
validate n = algo `mod` 10 == 0
    where algo = sumDigits . doubleEveryOther . toDigits $ n

