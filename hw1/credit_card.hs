-- Exercise 1

toDigits :: Integer -> [Integer]
toDigits n
  | n <= 0 = []
  | n < 10 = [n]
  | n >= 10 = toDigits (n `div` 10) ++ [n `mod` 10]
  | otherwise = []

toDigitsRev :: Integer -> [Integer]
toDigitsRev n = reverse (toDigits n)

-- Exercise 2
doubleEveryOtherLeft :: [Integer] -> [Integer]
doubleEveryOtherLeft l = case l of
  (x:y:li) -> x : 2 * y : doubleEveryOtherLeft li
  a -> a

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther l = reverse (doubleEveryOtherLeft (reverse l))

-- Exercise 3
sumDigits :: [Integer] -> Integer
sumDigits l = case l of
  (x:li) -> x + sumDigits li
  [] -> 0

-- Exercise 4
breakUpDigits :: [Integer] -> [Integer]
breakUpDigits l = case l of
  (n:li) -> toDigits(n) ++ breakUpDigits(li)
  [] -> []

validate :: Integer -> Bool
validate n = sumDigits (breakUpDigits (doubleEveryOther (toDigits n))) `mod` 10 == 0
