module Main where

import Control.Applicative (liftA2)

newtype Reader r a = Reader { runReader :: r -> a }

instance Functor (Reader r) where
  fmap f (Reader g) = Reader (f . g)

instance Applicative (Reader r) where
  pure x = Reader (\_ -> x)
  liftA2 f (Reader g) (Reader h) = Reader (\r -> f (g r) (h r))

instance Monad (Reader r) where
  (Reader g) >>= f = Reader (\r -> runReader (f (g r)) r)

ask :: Reader r r
ask = Reader id

asks :: (r -> a) -> Reader r a
asks f = Reader f

local :: (r -> r) -> Reader r a -> Reader r a
local f (Reader g) = Reader (g . f)

data BankConfig = BankConfig
  { interestRate   :: Double
  , transactionFee :: Int
  , minimumBalance :: Int
  } deriving (Show)

data Account = Account
  { accountId :: String
  , balance   :: Int
  } deriving (Show)

calculateInterest :: Account -> Reader BankConfig Int
calculateInterest acc = do
  rate <- asks interestRate
  return (floor (fromIntegral (balance acc) * rate))

applyTransactionFee :: Account -> Reader BankConfig Account
applyTransactionFee acc = do
  fee <- asks transactionFee
  return (acc { balance = balance acc - fee })

checkMinimumBalance :: Account -> Reader BankConfig Bool
checkMinimumBalance acc = do
  minBal <- asks minimumBalance
  return (balance acc >= minBal)

processAccount :: Account -> Reader BankConfig (Account, Int, Bool)
processAccount acc = do
  newAcc   <- applyTransactionFee acc
  interest <- calculateInterest acc
  meetsMin <- checkMinimumBalance acc
  return (newAcc, interest, meetsMin)

main :: IO ()
main = do
  let cfg = BankConfig { interestRate = 0.05, transactionFee = 2, minimumBalance = 100 }
  let acc = Account { accountId = "A-001", balance = 1000 }
  let result = runReader (processAccount acc) cfg
  print result

  let acc2 = Account { accountId = "A-002", balance = 50 }
  print (runReader (processAccount acc2) cfg)

  print (runReader (calculateInterest acc) cfg)
  print (runReader (applyTransactionFee acc) cfg)
  print (runReader (checkMinimumBalance acc) cfg)

  print (runReader ask cfg)
  print (runReader (asks interestRate) cfg)
  print (runReader (local (\c -> c { transactionFee = 10 }) (applyTransactionFee acc)) cfg)