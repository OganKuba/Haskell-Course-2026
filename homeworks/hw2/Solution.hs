module Exercises where

import Data.Foldable (toList)
import Data.Monoid (Sum(..))

data Sequence a = Empty | Single a | Append (Sequence a) (Sequence a)
    deriving (Show)

-- 1. Functor

instance Functor Sequence where
    fmap _ Empty        = Empty
    fmap f (Single x)   = Single (f x)
    fmap f (Append l r) = Append (fmap f l) (fmap f r)

-- 2. Foldable

instance Foldable Sequence where
    foldMap _ Empty        = mempty
    foldMap f (Single x)   = f x
    foldMap f (Append l r) = foldMap f l <> foldMap f r

seqToList :: Sequence a -> [a]
seqToList = toList

seqLength :: Sequence a -> Int
seqLength = getSum . foldMap (const (Sum 1))

-- 3. Semigroup and Monoid

instance Semigroup (Sequence a) where
    (<>) = Append

instance Monoid (Sequence a) where
    mempty = Empty

-- 4. tailElem

tailElem :: Eq a => a -> Sequence a -> Bool
tailElem x seq0 = go [seq0]
  where
    go []                   = False
    go (Empty : stack)      = go stack
    go (Single y : stack)   = x == y || go stack
    go (Append l r : stack) = go (l : r : stack)

-- 5. tailToList

tailToList :: Sequence a -> [a]
tailToList seq0 = go [] [seq0]
  where
    go acc []                   = reverse acc
    go acc (Empty : stack)      = go acc stack
    go acc (Single x : stack)   = go (x : acc) stack
    go acc (Append l r : stack) = go acc (l : r : stack)

-- 6. tailRPN

data Token = TNum Int | TAdd | TSub | TMul | TDiv
    deriving (Show)

tailRPN :: [Token] -> Maybe Int
tailRPN tokens = go tokens []
  where
    go [] [result]             = Just result
    go [] _                    = Nothing
    go (TNum n : ts) stack     = go ts (n : stack)
    go (op : ts) (b : a : stack) = case applyOp op a b of
        Nothing -> Nothing
        Just v  -> go ts (v : stack)
    go _ _                     = Nothing

    applyOp TAdd a b = Just (a + b)
    applyOp TSub a b = Just (a - b)
    applyOp TMul a b = Just (a * b)
    applyOp TDiv a b
        | b == 0    = Nothing
        | otherwise = Just (div a b)
    applyOp _ _ _   = Nothing

-- 7a. myReverse

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

-- 7b. myTakeWhile

myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile p = foldr (\x acc -> if p x then x : acc else []) []

-- 7c. decimal

decimal :: [Int] -> Int
decimal = foldl (\acc d -> acc * 10 + d) 0

-- 8a. encode

encode :: Eq a => [a] -> [(a, Int)]
encode = foldr step []
  where
    step x ((y, n) : rest)
        | x == y    = (y, n + 1) : rest
    step x rest     = (x, 1) : rest

-- 8b. decode

decode :: [(a, Int)] -> [a]
decode = foldr (\(x, n) acc -> replicate n x ++ acc) []

-- Testy

main :: IO ()
main = do
    let seq1 = Append (Append (Single 1) (Single 2)) (Append (Single 3) Empty)
    let seq2 = Append (Single 10) (Single 20)

    putStrLn "=== 1. Functor ==="
    print $ seqToList (fmap (*10) seq1)         -- [10,20,30]
    print $ seqToList (fmap (+1) seq2)           -- [11,21]

    putStrLn "\n=== 2. Foldable ==="
    print $ seqToList seq1                       -- [1,2,3]
    print $ seqLength seq1                       -- 3
    print $ seqLength Empty                      -- 0
    print $ sum seq1                             -- 6

    putStrLn "\n=== 3. Semigroup / Monoid ==="
    print $ seqToList (seq1 <> seq2)             -- [1,2,3,10,20]
    print $ seqToList (mempty <> seq1)           -- [1,2,3]
    print $ seqToList (seq1 <> mempty)           -- [1,2,3]

    putStrLn "\n=== 4. tailElem ==="
    print $ tailElem 2 seq1                      -- True
    print $ tailElem 5 seq1                      -- False
    print $ tailElem 20 seq2                     -- True
    print $ tailElem 99 Empty                    -- False

    putStrLn "\n=== 5. tailToList ==="
    print $ tailToList seq1                      -- [1,2,3]
    print $ tailToList (Empty :: Sequence Int)   -- []
    print $ tailToList seq2                      -- [10,20]

    putStrLn "\n=== 6. tailRPN ==="
    print $ tailRPN [TNum 3, TNum 4, TAdd]                     -- Just 7
    print $ tailRPN [TNum 5, TNum 3, TSub]                     -- Just 2
    print $ tailRPN [TNum 2, TNum 3, TNum 4, TAdd, TMul]       -- Just 14
    print $ tailRPN [TNum 10, TNum 0, TDiv]                    -- Nothing
    print $ tailRPN [TNum 1, TNum 2]                           -- Nothing
    print $ tailRPN [TAdd]                                     -- Nothing
    print $ tailRPN []                                         -- Nothing

    putStrLn "\n=== 7a. myReverse ==="
    print $ myReverse [1,2,3,4,5]               -- [5,4,3,2,1]
    print $ myReverse "hello"                    -- "olleh"
    print $ myReverse ([] :: [Int])              -- []

    putStrLn "\n=== 7b. myTakeWhile ==="
    print $ myTakeWhile even [2,4,3,6]           -- [2,4]
    print $ myTakeWhile (>0) [1,2,0,3]           -- [1,2]
    print $ myTakeWhile even [1,2,3]             -- []

    putStrLn "\n=== 7c. decimal ==="
    print $ decimal [1,2,3]                      -- 123
    print $ decimal [0]                          -- 0
    print $ decimal [9,9,9]                      -- 999

    putStrLn "\n=== 8a. encode ==="
    print $ encode "aaabccca"                    -- [('a',3),('b',1),('c',3),('a',1)]
    print $ encode "aabb"                        -- [('a',2),('b',2)]
    print $ encode ""                            -- []

    putStrLn "\n=== 8b. decode ==="
    print $ decode [('a',3),('b',1),('c',3),('a',1)]   -- "aaabccca"
    print $ decode [('x',5)]                            -- "xxxxx"
    print $ decode ([] :: [(Char, Int)])                 -- ""

    putStrLn "\n=== Wszystko OK ==="