module Main where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.List (permutations)
import Control.Monad (guard, mapM)
import Control.Monad.Writer

type Pos = (Int, Int)
data Dir = N | S | E | W deriving (Eq, Ord, Show)
type Maze = Map Pos (Map Dir Pos)

move :: Maze -> Pos -> Dir -> Maybe Pos
move maze pos dir = do
  neighbours <- Map.lookup pos maze
  Map.lookup dir neighbours

followPath :: Maze -> Pos -> [Dir] -> Maybe Pos
followPath _ pos [] = Just pos
followPath maze pos (d:ds) = do
  next <- move maze pos d
  followPath maze next ds

safePath :: Maze -> Pos -> [Dir] -> Maybe [Pos]
safePath _ pos [] = Just [pos]
safePath maze pos (d:ds) = do
  next <- move maze pos d
  rest <- safePath maze next ds
  return (pos : rest)

type Key = Map Char Char

decrypt :: Key -> String -> Maybe String
decrypt key = traverse (`Map.lookup` key)

decryptWords :: Key -> [String] -> Maybe [String]
decryptWords key = traverse (decrypt key)

type Guest = String
type Conflict = (Guest, Guest)

seatings :: [Guest] -> [Conflict] -> [[Guest]]
seatings guests conflicts = do
  perm <- permutations guests
  guard (validSeating perm conflicts)
  return perm

validSeating :: [Guest] -> [Conflict] -> Bool
validSeating [] _ = True
validSeating xs conflicts = all ok pairs
  where
    pairs = zip xs (tail xs ++ [head xs])
    ok (a, b) = not (any (\(x, y) -> (x == a && y == b) || (x == b && y == a)) conflicts)

data Result a = Failure String | Success a [String] deriving Show

instance Functor Result where
  fmap _ (Failure msg) = Failure msg
  fmap f (Success x ws) = Success (f x) ws

instance Applicative Result where
  pure x = Success x []
  (Failure msg) <*> _ = Failure msg
  _ <*> (Failure msg) = Failure msg
  (Success f ws1) <*> (Success x ws2) = Success (f x) (ws1 ++ ws2)

instance Monad Result where
  return = pure
  (Failure msg) >>= _ = Failure msg
  (Success x ws1) >>= f = case f x of
    Failure msg -> Failure msg
    Success y ws2 -> Success y (ws1 ++ ws2)

warn :: String -> Result ()
warn msg = Success () [msg]

failure :: String -> Result a
failure = Failure

validateAge :: Int -> Result Int
validateAge age
  | age < 0 = failure ("Negative age: " ++ show age)
  | age > 150 = do
      warn ("Suspicious age: " ++ show age)
      return age
  | otherwise = return age

validateAges :: [Int] -> Result [Int]
validateAges = mapM validateAge

data Expr = Lit Int | Add Expr Expr | Mul Expr Expr | Neg Expr deriving Show

simplify :: Expr -> Writer [String] Expr
simplify (Lit n) = return (Lit n)
simplify (Add e1 e2) = do
  s1 <- simplify e1
  s2 <- simplify e2
  case (s1, s2) of
    (Lit 0, e) -> do
      tell ["Add identity: 0 + e -> e"]
      return e
    (e, Lit 0) -> do
      tell ["Add identity: e + 0 -> e"]
      return e
    (Lit a, Lit b) -> do
      tell ["Constant folding: " ++ show a ++ " + " ++ show b ++ " -> " ++ show (a + b)]
      return (Lit (a + b))
    _ -> return (Add s1 s2)
simplify (Mul e1 e2) = do
  s1 <- simplify e1
  s2 <- simplify e2
  case (s1, s2) of
    (Lit 0, _) -> do
      tell ["Zero absorption: 0 * e -> 0"]
      return (Lit 0)
    (_, Lit 0) -> do
      tell ["Zero absorption: e * 0 -> 0"]
      return (Lit 0)
    (Lit 1, e) -> do
      tell ["Mul identity: 1 * e -> e"]
      return e
    (e, Lit 1) -> do
      tell ["Mul identity: e * 1 -> e"]
      return e
    (Lit a, Lit b) -> do
      tell ["Constant folding: " ++ show a ++ " * " ++ show b ++ " -> " ++ show (a * b)]
      return (Lit (a * b))
    _ -> return (Mul s1 s2)
simplify (Neg e) = do
  s <- simplify e
  case s of
    Neg inner -> do
      tell ["Double negation: -(-e) -> e"]
      return inner
    _ -> return (Neg s)

newtype ZipList a = ZipList { getZipList :: [a] } deriving (Show)

instance Functor ZipList where
  fmap f (ZipList xs) = ZipList (map f xs)

instance Applicative ZipList where
  pure x = ZipList (repeat x)
  (ZipList fs) <*> (ZipList xs) = ZipList (zipWith ($) fs xs)

testMaze :: Maze
testMaze = Map.fromList
  [ ((0,0), Map.fromList [(E, (1,0)), (N, (0,1))])
  , ((1,0), Map.fromList [(W, (0,0)), (N, (1,1))])
  , ((0,1), Map.fromList [(S, (0,0)), (E, (1,1))])
  , ((1,1), Map.fromList [(S, (1,0)), (W, (0,1))])
  ]

testKey :: Key
testKey = Map.fromList [('a','h'), ('b','e'), ('c','l'), ('d','o')]

main :: IO ()
main = do
  putStrLn "=== Maze ==="
  putStrLn $ "move (0,0) E:        " ++ show (move testMaze (0,0) E)
  putStrLn $ "move (0,0) S:        " ++ show (move testMaze (0,0) S)
  putStrLn $ "followPath [E,N]:    " ++ show (followPath testMaze (0,0) [E,N])
  putStrLn $ "followPath [E,S]:    " ++ show (followPath testMaze (0,0) [E,S])
  putStrLn $ "safePath [E,N,W]:    " ++ show (safePath testMaze (0,0) [E,N,W])
  putStrLn $ "safePath [E,S]:      " ++ show (safePath testMaze (0,0) [E,S])

  putStrLn "\n=== Decrypt ==="
  putStrLn $ "decrypt \"abcd\":      " ++ show (decrypt testKey "abcd")
  putStrLn $ "decrypt \"abxd\":      " ++ show (decrypt testKey "abxd")
  putStrLn $ "decryptWords ok:     " ++ show (decryptWords testKey ["ab", "cd"])
  putStrLn $ "decryptWords fail:   " ++ show (decryptWords testKey ["ab", "cz"])

  putStrLn "\n=== Seatings ==="
  let guests = ["A", "B", "C", "D"]
  let conflicts = [("A", "B")]
  let results = seatings guests conflicts
  putStrLn $ "Number of valid:     " ++ show (length results)
  putStrLn $ "First valid:         " ++ show (head results)

  putStrLn "\n=== Result monad ==="
  putStrLn $ "validateAge 25:      " ++ show (validateAge 25)
  putStrLn $ "validateAge (-5):    " ++ show (validateAge (-5))
  putStrLn $ "validateAge 200:     " ++ show (validateAge 200)
  putStrLn $ "validateAges mixed:  " ++ show (validateAges [30, 200, 50, 180])
  putStrLn $ "validateAges fail:   " ++ show (validateAges [30, -1, 50])

  putStrLn "\n=== Simplify ==="
  let expr1 = Add (Lit 0) (Mul (Lit 1) (Lit 5))
  let (r1, log1) = runWriter (simplify expr1)
  putStrLn $ "expr: " ++ show expr1
  putStrLn $ "result: " ++ show r1
  putStrLn $ "log: " ++ show log1

  let expr2 = Neg (Neg (Add (Lit 2) (Lit 3)))
  let (r2, log2) = runWriter (simplify expr2)
  putStrLn $ "expr: " ++ show expr2
  putStrLn $ "result: " ++ show r2
  putStrLn $ "log: " ++ show log2

  putStrLn "\n=== ZipList ==="
  let z1 = pure id <*> ZipList [1,2,3 :: Int]
  putStrLn $ "pure id <*> [1,2,3]: " ++ show (getZipList z1)
  let z2 = pure (+) <*> ZipList [1,2,3 :: Int] <*> ZipList [10,20,30]
  putStrLn $ "pure (+) <*> ...:    " ++ show (getZipList z2)

  putStrLn "\n=== All tests done ==="