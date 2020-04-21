#!/usr/bin/env runhaskell
-- | Calculations for FIRE (financial independence, retire early)
module Main where
import           Data.Decimal (Decimal(..), DecimalRaw(..), roundTo, divide)
import           Data.Either (fromRight)
import qualified Data.List as List
import           Data.Text (pack)
import           Data.Time.Calendar (Day)
import           Data.Time.Clock (UTCTime(utctDay), getCurrentTime)
import           Hledger

data Config = Config
  { age :: Decimal -- ^ How the heck do i convert btw Decimal and Integer?
  }

main = do
  let cfg = Config { age = 27 }
  j <- getJournal
  today <- getCurrentTime >>= return . utctDay
  say [ "savings rate:", show $ savingsRate j today ]
  say [ "target fund:", show $ targetFund j today ]
  let n = whenFreedom j today
  say [ "when free:", show $ n, "months"
      , "(I'll be", show $ roundTo 1 $ (n/12) + (age cfg), "years old)"
      ]

say = putStrLn . unwords

getJournal :: IO Journal
getJournal = do
  jp <- defaultJournalPath
  let opts = definputopts { auto_ = True }
  ej <- readJournalFile opts jp
  return $ fromRight undefined ej

-- | Helper for getting the total out of a balance report.
getTotal :: Journal -> Day -> String -> Quantity
getTotal j d q = head $ map aquantity $ total
  where
    opts = defreportopts { balancetype_ = CumulativeChange }
    (query, _) = parseQuery d $ pack q
    (_, (Mixed total)) = balanceReport opts query j

-- | These are the accounts that I consider a part of my savings and not my
-- cash-spending accounts.
savingsAccounts :: [String]
savingsAccounts =
  [ "as:me:save", "as:me:vest" ]

-- | Savings rate is a FIRE staple. Basically take your savings and divide it by
-- your income on a monthly basis.
--
savingsRate :: Journal -> Day -> Quantity
savingsRate j d = roundTo 2 $ allSavings / (- allIncome)
  -- gotta flip the sign because income is negative
  where
    allSavings = getTotal j d query
    query = List.intercalate " " $ savingsAccounts ++ ["cur:USD", "-p 'from 2019-11-01'"]
    allIncome = getTotal j d "^in"

-- | The target fund is simply 25x your annual expenditure.
--
-- This is going to be incomplete until I have a full year of
-- expenses.. currently, I just use my most recent quarter times 4 as a proxy
-- for the yearly expenses.
--
-- Assumptions: 4% withdrawal rate, 3-5% return on investments.
--
targetFund :: Journal -> Day -> Quantity
targetFund j d = 25 * yearlyExpenses
  where
    yearlyExpenses = 4 * quarterlyExpenses
    quarterlyExpenses = sum $ map aquantity $ total
    (query, _) = parseQuery d $ pack "^ex -p lastquarter cur:USD"
    (_, (Mixed total)) = balanceReport opts query j
    opts = defreportopts

-- | How long until I can live off of my savings and investment returns?
--
-- Return integer is number of months until I'm free.
--
whenFreedom :: Journal -> Day -> Quantity
whenFreedom j d = roundTo 1 $ targetFund j d / monthlySavings
  where
    monthlySavings = sum $ map (getTotal j d) $ map appendMonthly savingsAccounts
    appendMonthly s = s ++ " --monthly"
