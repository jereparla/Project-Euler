import Data.List
import System.IO.Unsafe
import Data.Numbers.Primes

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

parseNumber = do
    str <- readFile "../util/number.txt"
    return $ map (read :: String -> Int) . lines . intersperse '\n' $ filter (/= '\n') str

splitIn13 :: [Int] -> [[Int]]
splitIn13 (x:xs) | length (x:xs) >= 14 =  take 13 (x:xs) : splitIn13 xs
                 | otherwise = [(x:xs)]

splitParsed = splitIn13 $ unsafePerformIO parseNumber --AVOID TO DO THIS

multiply13 = map (foldl (*) 1) splitParsed

max13Product = maximum multiply13

problem8 :: IO ()
problem8 = print max13Product

-- Problem 9 --

pythagoras = let get1 (a,_,_) = a
                 get2 (_,b,_) = b
                 thrd (_,_,c) = c          
             in get1 triplet * get2 triplet * thrd triplet
             where triplet = head [(a,b,c) | a <- [1..1000], b <- [1..1000], let c = sqrt(a^2 + b^2), a < b && b < c, a + b + c == 1000] 

problem9 :: IO ()
problem9 = print pythagoras

-- Problem 10 --

problem10 :: IO ()
problem10 = print sumPrimes