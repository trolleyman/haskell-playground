import Data.Ratio

piString :: String
piString = digitsToString $ ratioToDigits $ calcPi

everyOther :: Int -> [(Int, Int)]
everyOther n = (n, n+2) : everyOther (n+4)

-- sum fucks everything up.
calcPi :: Ratio Int
calcPi = foldl (+) (0 % 1) $ map calcPi' (everyOther 1)

calcPi' :: (Int, Int) -> Ratio Int
calcPi' (x, y) = (4 % x) - (4 % y)

digitsToString :: [Int] -> String
digitsToString [] = ""
digitsToString (x:xs) = show x ++ "." ++ map toDigit xs 

toDigit :: Int -> Char
toDigit 0 = '0'
toDigit 1 = '1'
toDigit 2 = '2'
toDigit 3 = '3'
toDigit 4 = '4'
toDigit 5 = '5'
toDigit 6 = '6'
toDigit 7 = '7'
toDigit 8 = '8'
toDigit 9 = '9' 

divDigits :: Int -> Int -> [Int]
divDigits x y = x `div` y : divDigits (10 * (x `mod` y)) y

ratioToDigits :: Ratio Int -> [Int]
ratioToDigits r = divDigits n d
    where n = numerator r
          d = denominator r
