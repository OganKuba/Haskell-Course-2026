module Exercises where

goldbachPairs :: Int -> [(Int, Int)]
goldbachPairs n =
  [ (p, n - p)
  | p <- [2 .. n `div` 2]
  , isPrime p
  , isPrime (n - p)
  ]

coprimePairs :: [Int] -> [(Int, Int)]
coprimePairs xs =
  [ (x, y)
  | (i, x) <- zip [0..] xs
  , (j, y) <- zip [0..] xs
  , i < j
  , gcd x y == 1
  ]

sieve :: [Int] -> [Int]
sieve []     = []
sieve (p:xs) = p : sieve [ x | x <- xs, x `mod` p /= 0 ]

primesTo :: Int -> [Int]
primesTo n = sieve [2..n]

isPrime :: Int -> Bool
isPrime n = n >= 2 && n `elem` primesTo n

matMul :: [[Int]] -> [[Int]] -> [[Int]]
matMul a b =
  [ [ sum [ (a !! i) !! k * (b !! k) !! j | k <- [0 .. p - 1] ]
    | j <- [0 .. n - 1]
    ]
  | i <- [0 .. m - 1]
  ]
  where
    m = length a
    p = length (head a)
    n = length (head b)

permutations :: Int -> [a] -> [[a]]
permutations 0 _  = [[]]
permutations _ [] = []
permutations k xs =
  [ x : rest
  | (i, x) <- zip [0..] xs
  , rest   <- permutations (k - 1) (removeAt i xs)
  ]
  where
    removeAt i ys = take i ys ++ drop (i + 1) ys

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
  | x < y    = x : merge xs (y:ys)
  | x > y    = y : merge (x:xs) ys
  | otherwise = x : merge xs ys

hamming :: [Integer]
hamming = 1 : merge (map (*2) hamming)
                     (merge (map (*3) hamming)
                            (map (*5) hamming))


power :: Int -> Int -> Int
power b e = go 1 b e
  where
    go :: Int -> Int -> Int -> Int
    go !acc _    0 = acc
    go !acc base n = go (acc * base) base (n - 1)


listMaxSeq :: [Int] -> Int
listMaxSeq []     = error "listMaxSeq: empty list"
listMaxSeq (x:xs) = go x xs
  where
    go acc []     = acc
    go acc (y:ys) = let acc' = max acc y
                    in  acc' `seq` go acc' ys

listMaxBang :: [Int] -> Int
listMaxBang []     = error "listMaxBang: empty list"
listMaxBang (x:xs) = go x xs
  where
    go !acc []     = acc
    go !acc (y:ys) = go (max acc y) ys


primes :: [Int]
primes = sieve [2..]

isPrime' :: Int -> Bool
isPrime' n
  | n < 2     = False
  | otherwise = head (dropWhile (< n) primes) == n

meanNaive :: [Double] -> Double
meanNaive xs = go 0 0 xs
  where
    go s l []       = s / l
    go s l (x:rest) = go (s + x) (l + 1) rest

meanStrict :: [Double] -> Double
meanStrict xs = go 0 0 xs
  where
    go !s !l []       = s / l
    go !s !l (x:rest) = go (s + x) (l + 1) rest

meanAndVariance :: [Double] -> (Double, Double)
meanAndVariance xs = let (s, sq, l) = go 0 0 0 xs
                         mu = s / l
                     in  (mu, sq / l - mu * mu)
  where
    go !s !sq !l []       = (s, sq, l)
    go !s !sq !l (x:rest) = go (s + x) (sq + x * x) (l + 1) rest


main :: IO ()
main = do
  putStrLn "=== A1: Goldbach Pairs ==="
  putStrLn "goldbachPairs 28:"
  print (goldbachPairs 28)
  putStrLn ""

  putStrLn "=== A2: Coprime Pairs ==="
  putStrLn "coprimePairs [6, 10, 15, 7]:"
  print (coprimePairs [6, 10, 15, 7])
  putStrLn ""

  putStrLn "=== A3: Sieve of Eratosthenes ==="
  putStrLn "primesTo 50:"
  print (primesTo 50)
  putStrLn "isPrime 37:"
  print (isPrime 37)
  putStrLn ""

  putStrLn "=== A4: Matrix Multiplication ==="
  let a = [[1,2],[3,4]]
      b = [[5,6],[7,8]]
  putStrLn "[[1,2],[3,4]] * [[5,6],[7,8]]:"
  print (matMul a b)
  putStrLn ""

  putStrLn "=== A5: Permutations ==="
  putStrLn "permutations 2 [1,2,3]:"
  print (permutations 2 [1,2,3])
  putStrLn ""

  putStrLn "=== B1: Hamming Numbers ==="
  putStrLn "First 20 Hamming numbers:"
  print (take 20 hamming)
  putStrLn ""

  putStrLn "=== B2: Integer Power (Bang Patterns) ==="
  putStrLn "power 2 10:"
  print (power 2 10)
  putStrLn ""

  putStrLn "=== B3: Running Maximum ==="
  putStrLn "listMaxSeq [3,1,4,1,5,9,2,6]:"
  print (listMaxSeq [3,1,4,1,5,9,2,6])
  putStrLn "listMaxBang [3,1,4,1,5,9,2,6]:"
  print (listMaxBang [3,1,4,1,5,9,2,6])
  putStrLn ""

  putStrLn "=== B4: Infinite Prime Stream ==="
  putStrLn "First 20 primes:"
  print (take 20 primes)
  putStrLn "isPrime' 97:"
  print (isPrime' 97)
  putStrLn ""

  putStrLn "=== B5: Strict Accumulation ==="
  putStrLn "meanNaive [1..100]:"
  print (meanNaive [1..100])
  putStrLn "meanStrict [1..100]:"
  print (meanStrict [1..100])
  putStrLn "meanAndVariance [1..100]:"
  print (meanAndVariance [1..100])