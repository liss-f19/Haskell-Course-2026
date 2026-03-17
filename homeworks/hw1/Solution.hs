{-# LANGUAGE BangPatterns #-}
import Data.Function

-- from Basics/src/Tutorials01.hs
isPrime :: Int -> Bool
isPrime n 
 | n <= 1 = False
 | n == 2 = True
 | even n = False -- not needed but, makes the function run faster
 | otherwise = all (/= 0) [ n `mod` i | i <- [2..s]] --  n is not div by i
  where 
    s = n & fromIntegral & sqrt & floor -- funky way of writing 
                                        -- s = floor $ sqrt $ fromIntegral n

-- Task 1. Goldbach pairs
goldbachPairs :: Int -> [(Int, Int)]
goldbachPairs n = 
  [ (p,q)
  | p <- [2..n]
  , q <- [p..n-p]
  , p + q == n
  , isPrime p
  , isPrime q
  ]  


-- Task 2. Coprime pairs
-- Note: conventionally xs is a list[values]
coprimePairs :: [Int] -> [(Int, Int)]
coprimePairs xs = 
  [
    (x,y)
  | x <- xs
  , y <- xs
  , x < y
  , gcd x y == 1
  ]


-- Task 3. Sieve of Eratosthenes
sieve :: [Int] -> [Int]
sieve [] = [] --base case
sieve (p:xs) = p:sieve[x|x<-xs, x `mod` p /= 0]

primesTo :: Int -> [Int]
primesTo n = sieve [2..n]

isPrimeSieve :: Int -> Bool
isPrimeSieve n
 | n < 2 = False
 | otherwise = n `elem` primesTo n


-- Task 4. Matrix multiplication
-- Helper for checking the number of rows in matrix is the same
allEqual :: [[a]] -> Bool
allEqual xs = all ((== length (head xs)) . length) xs



matMul :: [[Int]] -> [[Int]] -> [[Int]]
matMul a b
 | null a || null b = error "Matrices cant be empty"
 | not (allEqual a) = error "Matrix a - all rows should have the same length"
 | not (allEqual b) = error "Matrix b - all rows should have the same length"
 | length(head a) /= length b = error "Number of columns of matrix a should be == number of rows of matrix b"
 | otherwise = 
  let m = length a
      p = length(head a)
      n = length(head b)
  in [[sum [ a !! i !! k * b !! k !! j | k <- [0 .. p-1] ]
      | j <- [0..n-1]]
      | i <- [0..m-1]]


-- Task 5. Permutations
-- Helper
remove :: Eq a => a -> [a] -> [a]
remove _ [] = []
remove y (z:zs)
  | y == z    = zs
  | otherwise = z : remove y zs

-- Generate all k-element permutations
permutationsCustom :: Eq a => Int -> [a] -> [[a]]
permutationsCustom 0 _  = [[]]  -- base case
permutationsCustom _ [] = []
permutationsCustom k xs = 
  [ x : ps
  | x <- xs
  , ps <- permutationsCustom (k-1) (remove x xs)
  ]

-- Task 6. Hamming Numbers
-- a. Helper merge
merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
  | x < y     = x : merge xs (y:ys)
  | x > y     = y : merge (x:xs) ys
  | otherwise = x : merge xs ys  -- skip duplicate

-- b.hamming infinite list
hamming :: [Integer]
hamming = 1 : merge (map (2*) hamming)
                   (merge (map (3*) hamming) (map (5*) hamming))


-- Task 7. bang patterns
-- here ! enables not having chains of not evaluated things in accum
power :: Int -> Int -> Int
power b e
  | e < 0     = error "power: negative exponent"
  | otherwise = go e 1
  where 
    go 0 !acc = acc
    go n !acc = go (n-1) (acc * b)


-- Task 8. seq vs bang patterns
listMaxSeq :: [Int] -> Int
listMaxSeq [] = error "empty list"
listMaxSeq (x:xs) = go xs x
  where
    go []     acc = acc
    go (y:ys) acc = let acc' = max acc y
                    in acc' `seq` go ys acc'


listMaxBang :: [Int] -> Int
listMaxBang []     = error "empty list"
listMaxBang (x:xs) = go xs x
  where
    go []     !acc = acc
    go (y:ys) !acc = go ys (max acc y)

