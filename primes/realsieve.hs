-- Unassessed Learning Exercise.
--
-- Prime numbers using the Sieve method, without performing division.
-- Produces an infinite list. 
-- https://en.wikipedia.org/wiki/Sieve_of_Eratosthenes

-- We represent "crossed out" by False.
--
-- We start with no element crossed out:
--
-- 2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 ...
-- T  T  T  T  T  T  T  T  T  T  T  T  T  T  T  T  T  T  T ...
--
-- Look at the first element that is not crossed out, namely 2.
-- Keep it as it is, but cross out every second element
--
-- 2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 ...
-- T  T  F  T  F  T  F  T  F  T  F  T  F  T  F  T  F  T  F ...
--
-- Now we look at the next element which is not crossed out, namely 3.
-- Keep it as it is, but cross out every third element
--
-- 2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 ...
-- T  T  F  T  F  T  F  F  F  T  F  T  F  F  F  T  F  F  F ...
--
-- Now we look at the next element which is not crossed out, namely 5.
-- Keep it as it is, but cross out every fifth element
--
-- 2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 ...
-- T  T  F  T  F  T  F  F  F  T  F  T  F  F  F  T  F  F  F ...
--
-- And so on.
-- The elements that remain uncrossed are the prime numbers.
--
-- We don't need the numbers in the list.
-- An infinite list of booleans suffices. 
-- We start with the infinite lists of True's.
-- Then we repeatedly sieve it from 2:

sieve :: [Bool]
sieve = sievefrom 2 (repeat True)

-- To sieve an infinite list, we look at its head. 
--
-- If it is True, we output True, and we cross out every nth
-- element. We use an auxiliary function for this.
--
-- If it is False, we output False, and sieve from the tail,
-- increasing the position count n. 

sievefrom :: Int -> [Bool] -> [Bool]
sievefrom n (True  : xs) = True  : sievefrom (n+1) (cross n xs)
sievefrom n (False : xs) = False : sievefrom (n+1) xs

-- We now define cross n xs so that the result is xs with every nth
-- element crossed out. A helper function gets the count, which
-- decreases in recursive calls, and every time it reaches zero it
-- both crosses out and resets the counter to n-1 again.

cross :: Int -> [Bool] -> [Bool] 
cross n = cross' (n-1) 
  where 
    cross' 0 (x : xs) = False : cross' (n-1) xs
    cross' i (x : xs) = x     : cross' (i-1) xs

-- Is a given number n prime?

isprime ::  Int -> Bool
isprime n = sieve !! (n-2)

-- Now your task. Complete the definitons below so that the evaluation
-- of take 40 fsieve' should produce the list
-- [[],[],[2],[],[3,2],[],[2],[3],[5,2],[],[3,2],[],[7,2],[5,3],[2],[],[3,2],[],[5,2],[7,3],[11,2],[],[3,2],[5],[13,2],[3],[7,2],[],[5,3,2],[],[2],[11,3],[17,2],[7,5],[3,2],[],[19,2],[13,3],[5,2],[]]
-- 
-- An empty list at position n means that n+2 is prime (the first
-- position is zero).
--
-- A non-empty list gives the factors of n+2 without repetitions,
-- excluding 1 and n+2.
--
-- You should obtain this by modifying the above functions, so that
-- the crossing out adds a factor instead.
--
-- Replace the of "undefined" below by suitable code:

fsieve :: [[Int]]
fsieve = fsievefrom 2 (repeat [])

fsievefrom :: Int -> [[Int]] -> [[Int]]
fsievefrom n ([]:xs) = [] : fsievefrom (n+1) (fcross n xs)
fsievefrom n (x :xs) = x  : fsievefrom (n+1) xs

fcross :: Int -> [[Int]] -> [[Int]] 
fcross n = fcross' (n-1)
  where fcross' 0 (x:xs) = (n:x) : fcross' (n-1) xs
        fcross' i (x:xs) = x     : fcross' (i-1) xs

-- Factors of n excluding 1 and n, using fsieve:
factors ::  Int -> [Int]
factors n = fsieve !! (n-2)

-- Check primality using the function factors:
prime' :: Int -> Bool
prime' n = fsieve !! (n-2) == []

primes :: [Int]
primes = map fst $ filter snd $ zip [2..] sieve

primes' :: [Int]
primes' = map fst $ filter (\(_, a) -> a == []) listOfPrimeFactors

-- Use zip and fsieve to produce a table of numbers >= with their
-- lists of factors:
listOfPrimeFactors :: [(Int , [Int])]
listOfPrimeFactors = zip [2..] fsieve
