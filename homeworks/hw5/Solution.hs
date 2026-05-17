module Main where

import Control.Monad.State
import Control.Monad.IO.Class
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Char (toLower)
import System.Environment (getArgs)


data Instr = PUSH Int | POP | DUP | SWAP | ADD | MUL | NEG
  deriving (Show, Eq)

execInstr :: Instr -> State [Int] ()
execInstr (PUSH n) = modify (n :)
execInstr POP = do
  s <- get
  case s of
    (_:xs) -> put xs
    _      -> return ()
execInstr DUP = do
  s <- get
  case s of
    (x:xs) -> put (x:x:xs)
    _      -> return ()
execInstr SWAP = do
  s <- get
  case s of
    (x:y:xs) -> put (y:x:xs)
    _        -> return ()
execInstr ADD = do
  s <- get
  case s of
    (x:y:xs) -> put ((y + x):xs)
    _        -> return ()
execInstr MUL = do
  s <- get
  case s of
    (x:y:xs) -> put ((y * x):xs)
    _        -> return ()
execInstr NEG = do
  s <- get
  case s of
    (x:xs) -> put ((-x):xs)
    _      -> return ()

execProg :: [Instr] -> State [Int] ()
execProg = mapM_ execInstr

runProg :: [Instr] -> [Int]
runProg p = execState (execProg p) []


data Expr
  = Num Int
  | Var String
  | EAdd Expr Expr
  | EMul Expr Expr
  | ENeg Expr
  | Assign String Expr
  | Seq Expr Expr
  deriving (Show)

eval :: Expr -> State (Map String Int) Int
eval (Num n) = return n
eval (Var x) = do
  env <- get
  case Map.lookup x env of
    Just v  -> return v
    Nothing -> error ("Unbound variable: " ++ x)
eval (EAdd a b) = do
  va <- eval a
  vb <- eval b
  return (va + vb)
eval (EMul a b) = do
  va <- eval a
  vb <- eval b
  return (va * vb)
eval (ENeg a) = do
  va <- eval a
  return (negate va)
eval (Assign x e) = do
  v <- eval e
  modify (Map.insert x v)
  return v
eval (Seq a b) = do
  _ <- eval a
  eval b

runEval :: Expr -> Int
runEval e = evalState (eval e) Map.empty


editDistM :: String -> String -> Int -> Int -> State (Map (Int, Int) Int) Int
editDistM xs ys i j = do
  cache <- get
  case Map.lookup (i, j) cache of
    Just v  -> return v
    Nothing -> do
      result <- compute
      modify (Map.insert (i, j) result)
      return result
  where
    compute
      | i == 0 = return j
      | j == 0 = return i
      | xs !! (i - 1) == ys !! (j - 1) = editDistM xs ys (i - 1) (j - 1)
      | otherwise = do
          d1 <- editDistM xs ys (i - 1) j
          d2 <- editDistM xs ys i (j - 1)
          d3 <- editDistM xs ys (i - 1) (j - 1)
          return (1 + minimum [d1, d2, d3])

editDistance :: String -> String -> Int
editDistance xs ys =
  evalState (editDistM xs ys (length xs) (length ys)) Map.empty


data LocationType
  = Empty
  | Decision [(String, Int)]
  | Obstacle Int
  | Treasure Int
  | Trap Int
  | Goal
  deriving (Show)

data GameState = GameState
  { position :: Int
  , score    :: Int
  , energy   :: Int
  , board    :: Map Int LocationType
  , finished :: Bool
  } deriving (Show)

type AdventureGame a = StateT GameState IO a

defaultBoard :: Map Int LocationType
defaultBoard = Map.fromList
  [ (0,  Empty)
  , (1,  Empty)
  , (2,  Treasure 10)
  , (3,  Obstacle 2)
  , (4,  Decision [("safe path", 5), ("risky path", 7)])
  , (5,  Empty)
  , (6,  Trap 5)
  , (7,  Empty)
  , (8,  Treasure 15)
  , (9,  Obstacle 3)
  , (10, Decision [("forest", 11), ("river", 13)])
  , (11, Treasure 5)
  , (12, Trap 8)
  , (13, Empty)
  , (14, Empty)
  , (15, Goal)
  ]

initialState :: GameState
initialState = GameState
  { position = 0
  , score = 0
  , energy = 30
  , board = defaultBoard
  , finished = False
  }

getDiceRoll :: IO Int
getDiceRoll = do
  putStr ">> Roll the dice (1-6): "
  line <- getLine
  case reads line :: [(Int, String)] of
    [(n, "")] | n >= 1 && n <= 6 -> return n
    _ -> do
      putStrLn "!! Invalid input. Please enter a number between 1 and 6."
      getDiceRoll

displayGameState :: GameState -> IO ()
displayGameState gs = do
  putStrLn "============================================"
  putStrLn $ "  Position: " ++ show (position gs)
  putStrLn $ "  Score:    " ++ show (score gs)
  putStrLn $ "  Energy:   " ++ show (energy gs)
  putStrLn "============================================"

getPlayerChoice :: [String] -> IO String
getPlayerChoice opts = do
  putStrLn "-- Choose one of the available paths:"
  mapM_ (\(i, o) -> putStrLn $ "   " ++ show i ++ ". " ++ o) (zip [1::Int ..] opts)
  putStr ">> Your choice (number or name): "
  line <- getLine
  let trimmed = map toLower (dropWhile (== ' ') line)
  case reads line :: [(Int, String)] of
    [(n, "")] | n >= 1 && n <= length opts -> return (opts !! (n - 1))
    _ -> case filter (\o -> map toLower o == trimmed) opts of
      (x:_) -> return x
      []    -> do
        putStrLn "!! Invalid choice. Try again."
        getPlayerChoice opts

movePlayer :: Int -> AdventureGame Int
movePlayer roll = do
  gs <- get
  let newPos = position gs + roll
      newEnergy = energy gs - 1
  put gs { position = newPos, energy = newEnergy }
  liftIO $ putStrLn $ "** You move " ++ show roll ++ " spaces forward."
  return roll

makeDecision :: [String] -> AdventureGame String
makeDecision opts = do
  choice <- liftIO $ getPlayerChoice opts
  liftIO $ putStrLn $ "** You chose: " ++ choice
  return choice

handleLocation :: AdventureGame Bool
handleLocation = do
  gs <- get
  let loc = Map.lookup (position gs) (board gs)
  case loc of
    Nothing -> do
      liftIO $ putStrLn "** You step into an unknown area..."
      return False
    Just Empty -> do
      liftIO $ putStrLn "** An empty space. You rest a moment."
      return False
    Just (Treasure pts) -> do
      liftIO $ putStrLn $ "$$ You found a treasure worth " ++ show pts ++ " points!"
      modify $ \s -> s { score = score s + pts }
      return False
    Just (Trap pts) -> do
      liftIO $ putStrLn $ "!! A trap! You lose " ++ show pts ++ " points."
      modify $ \s -> s { score = max 0 (score s - pts) }
      return False
    Just (Obstacle back) -> do
      liftIO $ putStrLn $ "## An obstacle! You are pushed " ++ show back ++ " spaces back."
      modify $ \s -> s { position = max 0 (position s - back) }
      return False
    Just (Decision options) -> do
      let names = map fst options
      choice <- makeDecision names
      let newPos = case lookup choice options of
                     Just p  -> p
                     Nothing -> position gs
      modify $ \s -> s { position = newPos }
      liftIO $ putStrLn $ "** Path taken. You jump to position " ++ show newPos ++ "."
      return False
    Just Goal -> do
      liftIO $ putStrLn "@@ You reached the main treasure! Victory!"
      modify $ \s -> s { finished = True }
      return True

playTurn :: AdventureGame Bool
playTurn = do
  gs <- get
  liftIO $ displayGameState gs
  if energy gs <= 0
    then do
      liftIO $ putStrLn "** You collapsed from exhaustion. Game over."
      modify $ \s -> s { finished = True }
      return True
    else do
      roll <- liftIO getDiceRoll
      _ <- movePlayer roll
      gs2 <- get
      let maxPos = maximum (Map.keys (board gs2))
      if position gs2 > maxPos
        then do
          liftIO $ putStrLn "** You ran past the board. Stepping back to the last tile."
          modify $ \s -> s { position = maxPos }
        else return ()
      handleLocation

playGame :: AdventureGame ()
playGame = do
  done <- playTurn
  gs <- get
  if done || finished gs
    then do
      liftIO $ putStrLn "================ GAME ENDED ================"
      liftIO $ displayGameState gs
    else playGame

runTreasureHunters :: IO ()
runTreasureHunters = do
  putStrLn "############################################"
  putStrLn "##         TREASURE HUNTERS               ##"
  putStrLn "############################################"
  putStrLn "Reach the treasure with as many points as possible!"
  _ <- execStateT playGame initialState
  return ()


check :: (Eq a, Show a) => String -> a -> a -> IO Bool
check name got expected = do
  if got == expected
    then do
      putStrLn $ "[OK]   " ++ name
      return True
    else do
      putStrLn $ "[FAIL] " ++ name
      putStrLn $ "         expected: " ++ show expected
      putStrLn $ "         got:      " ++ show got
      return False

testStack :: IO [Bool]
testStack = do
  putStrLn "\n=== Stack machine tests ==="
  r1 <- check "3 + 4" (runProg [PUSH 3, PUSH 4, ADD]) [7]
  r2 <- check "5 * 6" (runProg [PUSH 5, PUSH 6, MUL]) [30]
  r3 <- check "neg 10" (runProg [PUSH 10, NEG]) [-10]
  r4 <- check "SWAP" (runProg [PUSH 1, PUSH 2, SWAP]) [1, 2]
  r5 <- check "DUP" (runProg [PUSH 7, DUP]) [7, 7]
  r6 <- check "POP" (runProg [PUSH 1, POP]) []
  r7 <- check "ADD on empty stack is skipped" (runProg [ADD]) []
  r8 <- check "ADD with one element is skipped" (runProg [PUSH 1, ADD]) [1]
  r9 <- check "POP on empty stack is skipped" (runProg [POP]) []
  r10 <- check "2 + (3 * 4)" (runProg [PUSH 2, PUSH 3, PUSH 4, MUL, ADD]) [14]
  r11 <- check "SWAP keeps both" (runProg [PUSH 10, PUSH 3, SWAP]) [10, 3]
  return [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11]

testEval :: IO [Bool]
testEval = do
  putStrLn "\n=== Expression evaluator tests ==="
  r1 <- check "Num 42" (runEval (Num 42)) 42
  r2 <- check "2 + 3" (runEval (EAdd (Num 2) (Num 3))) 5
  r3 <- check "2 * 3" (runEval (EMul (Num 2) (Num 3))) 6
  r4 <- check "Neg 7" (runEval (ENeg (Num 7))) (-7)
  let e1 = Seq (Assign "x" (Num 10)) (Var "x")
  r5 <- check "x = 10; x" (runEval e1) 10
  let e2 = Seq (Assign "x" (Num 5))
              (Seq (Assign "y" (Num 7))
                   (EAdd (Var "x") (Var "y")))
  r6 <- check "x=5; y=7; x+y" (runEval e2) 12
  let e3 = Seq (Assign "x" (Num 3))
              (Seq (Assign "x" (EMul (Var "x") (Num 4)))
                   (Var "x"))
  r7 <- check "mutation: x=3; x=x*4; x" (runEval e3) 12
  let e4 = Assign "z" (EAdd (Num 1) (Num 2))
  r8 <- check "Assign returns value" (runEval e4) 3
  return [r1, r2, r3, r4, r5, r6, r7, r8]

testEditDist :: IO [Bool]
testEditDist = do
  putStrLn "\n=== Edit distance tests ==="
  r1 <- check "kitten -> sitting" (editDistance "kitten" "sitting") 3
  r2 <- check "saturday -> sunday" (editDistance "saturday" "sunday") 3
  r3 <- check "empty -> abc" (editDistance "" "abc") 3
  r4 <- check "abc -> empty" (editDistance "abc" "") 3
  r5 <- check "equal strings" (editDistance "hello" "hello") 0
  r6 <- check "a -> b" (editDistance "a" "b") 1
  r7 <- check "intention -> execution" (editDistance "intention" "execution") 5
  r8 <- check "empty -> empty" (editDistance "" "") 0
  r9 <- check "horse -> ros" (editDistance "horse" "ros") 3
  return [r1, r2, r3, r4, r5, r6, r7, r8, r9]

testTreasureStructure :: IO [Bool]
testTreasureStructure = do
  putStrLn "\n=== Treasure Hunters - state structure tests ==="
  let s0 = initialState
  r1 <- check "initial position" (position s0) 0
  r2 <- check "initial score" (score s0) 0
  r3 <- check "initial energy" (energy s0) 30
  r4 <- check "not finished" (finished s0) False
  (moved, s1) <- runStateT (movePlayer 4) s0
  r5 <- check "movePlayer returns roll" moved 4
  r6 <- check "position after move" (position s1) 4
  r7 <- check "energy after move" (energy s1) 29
  let sTreasure = s0 { position = 2 }
  (_, sAfter) <- runStateT handleLocation sTreasure
  r8 <- check "treasure increases score" (score sAfter) 10
  let sTrap = s0 { position = 6, score = 20 }
  (_, sAfterTrap) <- runStateT handleLocation sTrap
  r9 <- check "trap decreases score" (score sAfterTrap) 15
  let sObst = s0 { position = 9 }
  (_, sAfterObst) <- runStateT handleLocation sObst
  r10 <- check "obstacle pushes back" (position sAfterObst) 6
  let sGoal = s0 { position = 15 }
  (reached, sAfterGoal) <- runStateT handleLocation sGoal
  r11 <- check "goal returns True" reached True
  r12 <- check "goal marks finished" (finished sAfterGoal) True
  return [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12]

runTests :: IO ()
runTests = do
  rs1 <- testStack
  rs2 <- testEval
  rs3 <- testEditDist
  rs4 <- testTreasureStructure
  let all_results = rs1 ++ rs2 ++ rs3 ++ rs4
      passed = length (filter id all_results)
      total  = length all_results
  putStrLn ""
  putStrLn "############################################"
  putStrLn $ "  Passed " ++ show passed ++ " / " ++ show total ++ " tests"
  putStrLn "############################################"
  if passed == total
    then putStrLn "  ALL TESTS PASSED"
    else putStrLn "  SOME TESTS FAILED"

main :: IO ()
main = do
  args <- getArgs
  case args of
    ("play":_) -> runTreasureHunters
    _          -> runTests