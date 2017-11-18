module CreditCard where

toDigits :: Integer -> [Integer]
toDigits x
    | x > 0 = toDigits (x `div` 10) ++ [x `mod` 10]
    | otherwise = []

toDigitsRev :: Integer -> [Integer]
toDigitsRev x = reverse (toDigits x)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs =
    let reverseListWithIndex = zip (reverse xs) [0..]
    in reverse (foldr (\x y -> let num = fst x in (if odd (snd x) then num * 2 else num) : y) [] reverseListWithIndex)

-- Other people solution
doubleEveryOther2 :: [Integer] -> [Integer]
doubleEveryOther2 = reverse . zipWith (*) (cycle [1,2]) . reverse

sumDigits :: [Integer] -> Integer
sumDigits = sum . concat . map toDigits

validate :: Integer -> Bool
validate cardNumber = (sumDigits . doubleEveryOther . toDigits $ cardNumber) `mod` 10 == 0