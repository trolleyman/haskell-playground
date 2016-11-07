primes1 :: [Int]
primes1 = primesHelper [2..]

primesHelper :: [Int] -> [Int]
primesHelper (x:xs) = x : primesHelper [y | y <- xs, y `mod` x /= 0]

primes2 :: [Int]
primes2 = map fst $ filter (\(_, a) -> a == []) $ zip [2..] $ primes2' 2 emptyInf

primes2' :: Int -> [[Int]] -> [[Int]]
primes2' _ []      = []
primes2' i ([]:xs) = [] : primes2' (i+1) (addTo xs i (i-1)) 
primes2' i (x :xs) = x  : primes2' (i+1) xs

emptyInf :: [[Int]]
emptyInf = repeat []

-- Input array, n, skip, output array
addTo :: [[Int]] -> Int -> Int -> [[Int]]
addTo []     _ _ = []
addTo (x:xs) n 0 = (n:x) : addTo xs n (n-1)
addTo (x:xs) n s = x     : addTo xs n (s-1)
