#!/usr/bin/env runhaskell

{-# LANGUAGE OverloadedStrings #-}

-- | Calculates and displays an overview of my finances.
module Main where

import Data.Decimal (Decimal (..), DecimalRaw (..), divide, roundTo)
import Data.Either (fromRight)
import Data.Function ((&))
import qualified Data.List as List
import Data.Text (Text, pack)
import qualified Data.Text as T
import qualified Data.Text.IO as IO
import Data.Time.Calendar (Day, toGregorian)
import Data.Time.Clock (UTCTime (utctDay), getCurrentTime)
import Hledger

data Config = Config
  { age :: Decimal
  }

main = do
  j <- getJournal
  today <- getCurrentTime >>= return . utctDay
  let bal = getTotal j today $ defreportopts
  let balUSD = getTotal j today $ defreportopts {value_ = inUsd}
  sec "cash balances"
  row "simple" (prn $ bal "^as:me:cash:simple status:! status:*") Nothing
  row "wallet" (prn $ bal "^as:me:cash:wallet") Nothing
  row "  disc" (prn $ bal "^li:me:cred:discover status:*") Nothing
  row "  citi" (prn $ bal "^li:me:cred:citi status:*") Nothing
  row "   btc" (prn $ bal "^as cur:BTC") Nothing

  sec "metrics"
  let netLiquid = roundTo 2 $ bal "^as:me:cash ^li:me:cred cur:USD"
  let netWorth = roundTo 2 $ balUSD "^as ^li"
  row "  in - ex" (prn $ bal "^in ^ex cur:USD") $ Just "keep this negative to make progress"
  row "cred load" (prn netLiquid) $ Just "net liquid: credit spending minus cash assets. keep it positive"
  row "net worth" (prn netWorth) Nothing
  row "    level" (pr $ level netWorth) Nothing

  sec "trivials"
  let trivialWorth = roundTo 2 $ trivial * netWorth
  let trivialLiquid = roundTo 2 $ trivial * netLiquid
  row "   net" (pr trivialWorth) Nothing
  row "liquid" (pr trivialLiquid) Nothing

  sec "fire"
  let (thisyear, _, _) = toGregorian today
  let cfg = Config {age = (fromInteger thisyear) - 1992}
  let n = whenFreedom j today
  let ageFree = roundTo 1 $ (n / 12) + (age cfg)
  row "savings rate" (pr $ savingsRate j today) Nothing
  row " target fund" (prn $ roundTo 2 $ targetFund j today) Nothing
  row "   when free" ((pr n) <> " months") $ Just $ "I'll be " <> pr ageFree <> " years old"

sec :: String -> IO ()
sec label = putStrLn $ "\n" <> label <> ":"

pr :: Show s => s -> Text
pr = pack . show

row :: Text -> Text -> Maybe Text -> IO ()
row label value Nothing = IO.putStrLn $ gap <> label <> ":" <> gap <> value
row label value (Just nb) = IO.putStrLn $ gap <> label <> ":" <> gap <> value <> gap <> "\t(" <> nb <> ")"

gap :: Text
gap = "  "

-- Pretty-print a number. From https://stackoverflow.com/a/61070523/1146898
prn :: Quantity -> Text
prn d = T.intercalate "." $ case T.splitOn "." $ T.pack $ show d of
  x : xs -> (T.pack . reverse . go . reverse . T.unpack) x : xs
  xs -> xs
  where
    go (x : y : z : []) = x : y : z : []
    go (x : y : z : ['-']) = x : y : z : ['-']
    go (x : y : z : xs) = x : y : z : ',' : go xs
    go xs = xs

-- | There's levels to life, a proxy metric for what level you're at is your net
-- worth rounded down to the nearest power of 10.
level :: Decimal -> Integer
level = floor . logBase 10 . realToFrac

-- | A trivial decision is one that is between 0.01% and 0.1% of the total. This
-- uses the upper bound of that range.
--
-- From <https://ofdollarsanddata.com/climbing-the-wealth-ladder/>
trivial :: Quantity
trivial = 0.001

inUsd :: Maybe ValuationType
inUsd = Just $ AtNow $ Just "USD"

getTotal :: Journal -> Day -> ReportOpts -> String -> Quantity
getTotal j d opts q = sum $ map aquantity $ total
  where
    opts' =
      opts
        { today_ = Just d
        }
    (query, _) = parseQuery d $ pack q
    (_, (Mixed total)) = balanceReport opts' query j

getJournal :: IO Journal
getJournal = do
  jp <- defaultJournalPath
  let opts = definputopts {auto_ = True}
  ej <- readJournalFile opts jp
  return $ fromRight undefined ej

-- | These are the accounts that I consider a part of my savings and not my
-- cash-spending accounts.
savingsAccounts :: [String]
savingsAccounts =
  ["as:me:save", "as:me:vest"]

-- | Savings rate is a FIRE staple. Basically take your savings and divide it by
-- your income on a monthly basis.
savingsRate :: Journal -> Day -> Quantity
savingsRate j d = roundTo 2 $ allSavings / allIncome
  where
    allSavings = getTotal j d (defreportopts {value_ = inUsd}) query
    query = List.intercalate " " $ savingsAccounts
    -- gotta flip the sign because income is negative
    allIncome = - getTotal j d (defreportopts {value_ = inUsd}) "^in"

-- | The target fund is simply 25x your annual expenditure.
--
-- This is going to be incomplete until I have a full year of
-- expenses.. currently, I just use my most recent quarter times 4 as a proxy
-- for the yearly expenses.
--
-- Assumptions: 4% withdrawal rate, 3-5% return on investments.
targetFund :: Journal -> Day -> Quantity
targetFund j d = 25 * yearlyExpenses
  where
    yearlyExpenses = sum $ map aquantity $ total
    (query, _) = parseQuery d $ pack "^ex"
    (_, (Mixed total)) = balanceReport opts query j
    opts =
      defreportopts
        { -- idk what the '2020 4' is for, but this actually results in the yearly
          -- report for some reason
          period_ = QuarterPeriod 2020 4,
          value_ = Just $ AtNow $ Just "USD",
          today_ = Just d
        }

-- | How long until I can live off of my savings and investment returns?
--
-- Return integer is number of months until I'm free.
whenFreedom :: Journal -> Day -> Quantity
whenFreedom j d = roundTo 1 $ targetFund j d / monthlySavings
  where
    (year, month, _) = toGregorian d
    -- I have data going back to 2018.12
    monthsSinceBeginning = fromInteger $ (year - 2019) * 12 + toInteger month + 1
    monthlySavings =
      savingsAccounts
        & map (getTotal j d (defreportopts {value_ = inUsd, period_ = MonthPeriod 2020 10}))
        & sum
        & \n -> (n / monthsSinceBeginning)

-- | Escape velocity:
--
-- In physics it is basically the movement of an object away from the earth,
-- calculated as:
--
-- v =  sqrt(2 * G * M / r)
--
-- where:
--
-- - G :: gravitational constant (inflation rate)
-- - M :: mass to be escaped from (liabilities)
-- - r :: distance from center of M (current net worth)
--
-- So, to translate that into my finances would be something like:
--
-- v = sqrt(2 * 3% * li / net_worth)
--
-- v = sqrt(2 * 3% * 20,000 / 100,000)
--
-- v = 0.1095
--
-- I don't know what this means... maybe my money must be growing at a 25% rate in
-- order to cover the debt? I need to think about this equation some more.
--
-- Basically, escape velocity will be when my assets are growing faster than my
-- debts. In order to know this, I need:
--
-- 1. the accrual of every liability
-- 2. the return on every investment i make
-- 3. accrual rate must be less than return rate
-- 4. my income must always be more than my expenses
--
-- Once I have all four conditions satisfied, then my finances will be in
-- correct order. The challenge then is to have a system that continually
-- satisfies the 4 conditions.
escapeVelocity :: Journal -> Quantity
escapeVelocity = undefined
