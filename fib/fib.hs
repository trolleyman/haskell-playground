
fib :: Int -> Integer
fib n = fibArr !! n

fibLazy :: Int -> Integer
fibLazy n = fibArrLazy !! n

fibArr :: [Integer]
fibArr = fibArr' 0 1

fibArr' :: Integer -> Integer -> [Integer]
fibArr' a b = f $! a + b
    where f x = x : fibArr' b x

fibArrLazy :: [Integer]
fibArrLazy = fibArrLazy' 0 1

fibArrLazy' :: Integer -> Integer -> [Integer]
fibArrLazy' a b = s : fibArrLazy' b s
    where s = a + b
