-- Problem 1 --

sum3or5 = sum [x | x <- [1..999], (x `mod` 3 == 0 || x `mod` 5 == 0)] 

problem1 :: IO ()
problem1 = print sum3or5

-- Problem 2 --

fib :: Int -> Int
fib 0 = 1
fib 1 = 2 
fib n = fib (n - 1) + fib (n - 2)

sumEvenFib = sum [x | x <- takeWhile (\x -> x < 4000000) (map fib [0,1..]), x `mod` 2 == 0]

problem2 :: IO ()
problem2 = print sumEvenFib

-- Problem 3 --

isPrime :: Int -> Bool 
isPrime 1 = False
isPrime 2 = True
isPrime n = length [x | x <- [2,3..ceiling (sqrt (fromIntegral n))], n `mod` x == 0] == 0

primeFactors :: Int -> [Int]
primeFactors 1 = []
primeFactors n | isPrime n == False = y : primeFactors (n `div` y)
               | otherwise = n : primeFactors 1
                where 
                    y = head [x | x <- filter isPrime [2,3.. ceiling (sqrt (fromIntegral n))], n `mod` x == 0]

largestPrimeFactor = (maximum (primeFactors (600851475143)))::Int

problem3 :: IO ()
problem3 = print largestPrimeFactor

-- Problem 4 --

isPalindrome :: Int -> Bool
isPalindrome n | reverse (show n) == show n = True
               | otherwise = False

largestPalindrome = maximum [x | y <- [100..999], z <- [100..999], let x = y * z, isPalindrome x == True]


problem4 :: IO ()
problem4 = print largestPalindrome

-- Problem 5 --

problem5 :: IO ()
problem5 = print (foldr1 lcm [1..20])

-- Problem 6 --

diffOfSums = ((foldl1 (+) [1..100])^2) - (foldl1 (+) (map (^2) [1..100]))

problem6 :: IO ()
problem6 = print diffOfSums


-- Problem 7 --

prime100001 = head (drop 10000 [x | x <- [2,3..], isPrime x])

--OR--

prime100001A = [x | x <- [2,3..], isPrime x] !! 10000

problem7 :: IO ()
problem7 = print prime100001

-- Problem 8 --




