#!/usr/bin/env runhaskell
-- | Calculates and displays an overview of my finances.
module Main where
import Hledger
import Data.Either (fromRight)
import Data.Time.Calendar (Day)
import Data.Time.Clock (UTCTime(utctDay), getCurrentTime)
import Data.Text (pack)

main = do
  j <- getJournal
  today <- getCurrentTime >>= return . utctDay
  let bal = getTotal j today
  sec "cash balances"
  row "simple" (bal "^as:me:cash:simple status:! status:*") Nothing
  row "wallet" (bal "^as:me:cash:wallet") Nothing
  row "disc" (bal "^li:me:cred:discover status:*") Nothing
  row "citi" (bal "^li:me:cred:citi status:*") Nothing
  row "btc" (bal "^as cur:BTC") Nothing

  sec "savings"
  row "simple" (bal "^as:me:save:simple") Nothing
  row "tosave" (bal "^li:me:save:base --auto") Nothing

  sec "metrics"
  row "in - ex" (bal "^in ^ex cur:USD -p thismonth") $ Just "keep this negative to make progress"
  let netLiquid = bal "^as:me:cash ^li:me:cred cur:USD --real"
  row "cred load" netLiquid $ Just "net liquid: credit spending minus cash assets. keep it positive"
  let netWorth = bal "^as ^li cur:USD --real"
  let trivialWorth = trivial * netWorth
  let trivialLiquid = trivial * netLiquid
  row "net worth" netWorth Nothing

  sec "trivials"
  row "net" trivialWorth Nothing
  row "liquid" trivialLiquid Nothing

sec label = putStrLn $ "\n" <> label <> ":"
row label value Nothing = putStrLn $ "\t" <> label <> ":\t" <> show value
row label value (Just nb) = putStrLn $ "\t" <> label <> ":\t" <> show value <> "\t\t(" <> nb <> ")"

-- | A trivial decision is one that is between 0.01% and 0.1% of the total. This
-- uses the upper bound of that range.
--
-- From <https://ofdollarsanddata.com/climbing-the-wealth-ladder/>
trivial = 0.001

getTotal :: Journal -> Day -> String -> Quantity
getTotal j d q = head $ map aquantity $ total
  where
    opts = defreportopts { balancetype_ = CumulativeChange, real_ = True }
    (query, _) = parseQuery d $ pack q
    (_, (Mixed total)) = balanceReport opts query j

getJournal :: IO Journal
getJournal = do
  jp <- defaultJournalPath
  let opts = definputopts { auto_ = True  }
  ej <- readJournalFile opts jp
  return $ fromRight undefined ej

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
-- v = sqrt(2 * 3% * 18534.54 / 17580.93)
--
-- v = 0.25
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
