#!/usr/bin/env runhaskell

{-# LANGUAGE OverloadedStrings #-}

-- | Calculates and displays an overview of my finances.
module Main where

import Data.Decimal (Decimal (..), DecimalRaw (..), divide, roundTo, roundTo', realFracToDecimal)
import Data.Either (fromRight)
import Data.Function ((&))
import qualified Data.List as List
import Data.Text (Text, pack)
import qualified Data.Text as T
import qualified Data.Text.IO as IO
import Data.Time.Calendar (Day, toGregorian)
import Data.Time.Clock (UTCTime (utctDay), getCurrentTime)
import Hledger

today :: IO Day
today = getCurrentTime >>= return . utctDay

-- | For running stuff in ghci
run :: (Journal -> Day -> a) -> IO a
run f = do
  j <- defaultJournal
  t <- today
  return $ f j t

main = do
  j <- defaultJournal
  t <- today
  let bal = getTotal j t $ defreportopts
  let balVal = getTotal j t $ defreportopts {value_ = inUsdNow}
  sec "cash balances"
  row "simple" (prn $ bal "^as:me:cash:simple status:! status:*") Nothing
  row "cashap" (prn $ bal "^as:me:cash:cashapp status:! status:*") Nothing
  row "wallet" (prn $ bal "^as:me:cash:wallet") Nothing
  row "  disc" (prn $ bal "^li:me:cred:discover status:*") Nothing
  row "  citi" (prn $ bal "^li:me:cred:citi status:*") Nothing
  let btcBal q = sum . map aquantity $ getTotalAmounts j t defreportopts q
  let btcBalUSD q = sum . map aquantity $ getTotalAmounts j t (defreportopts {value_ = inUsdNow}) q
  row "   btc" (prn $ btcBal "^as cur:BTC") (Just $ prn $ btcBalUSD "^as cur:BTC")

  sec "metrics"
  let netCash = bal "^as:me:cash ^li:me:cred cur:USD"
  let netWorth = balVal "^as ^li"
  row "  in - ex" (prn $ bal "^in ^ex" / monthsSinceBeginning t) $ Just "keep this negative to make progress"
  row "cred load" (prn netCash) $ Just "net cash: credit spending minus USD cash assets. keep it positive"
  row "net worth" (prn netWorth) Nothing
  row "    level" (pr $ level netWorth) (Just $ "+" <> (prn $ netWorth - (unlevel $ roundTo' floor 1 $ level netWorth)))
  let levelup n = level netWorth & (+n) & roundTo' floor 1 & unlevel & \target -> target - netWorth
  row "     next" (prn $ levelup 0.1) (Just $ prn $ roundTo' floor 1 $ level netWorth + 0.1)
  row "    nnext" (prn $ levelup 0.2) (Just $ prn $ roundTo' floor 1 $ level netWorth + 0.2)
  row "   nnnext" (prn $ levelup 0.3) (Just $ prn $ roundTo' floor 1 $ level netWorth + 0.3)

  sec "trivials"
  let trivialWorth = roundTo 2 $ trivial * netWorth
  let trivialCash = roundTo 2 $ trivial * netCash
  row "   net" (pr trivialWorth) Nothing
  row "  cash" (pr trivialCash) Nothing

  sec "fire"
  let (thisyear, _, _) = toGregorian t
  let age = (fromInteger thisyear) - 1992
  let n = whenFreedom j t
  let ageFree = roundTo 1 $ (n / 12) + age :: Decimal
  row "savings rate" (pr $ savingsRate j t) Nothing
  row " target fund" (prn $ targetFund j t) Nothing
  row "   when free" ((pr n) <> " months") $ Just $ "I'll be " <> pr ageFree <> " years old"

  sec "runway"
  let (nut, cash, months) = runway j t
  row "   nut" (prn nut) Nothing
  row "  cash" (prn cash) Nothing
  row "months" (prn months) Nothing

  sec "ramen"
  let (nut, cash, months) = ramen j t
  row "   nut" (prn nut) Nothing
  row "  cash" (prn cash) Nothing
  row "months" (prn months) Nothing

sec :: String -> IO ()
sec label = putStrLn $ "\n" <> label <> ":"

pr :: Show s => s -> Text
pr = pack . show

row :: Text -> Text -> Maybe Text -> IO ()
row label value Nothing = IO.putStrLn $ gap <> label <> ":" <> gap <> value
row label value (Just nb) = IO.putStrLn $ gap <> label <> ":" <> gap <> value <> gap <> "\t(" <> nb <> ")"

gap :: Text
gap = "  "

-- | Pretty-print a number. From https://stackoverflow.com/a/61070523/1146898
prn :: Quantity -> Text
prn d = T.intercalate "." $ case T.splitOn "." $ T.pack $ show $ roundTo 2 d of
  x : xs -> (T.pack . reverse . go . reverse . T.unpack) x : xs
  xs -> xs
  where
    go (x : y : z : []) = x : y : z : []
    go (x : y : z : ['-']) = x : y : z : ['-']
    go (x : y : z : xs) = x : y : z : ',' : go xs
    go xs = xs

-- | There's levels to life, a proxy metric for what level you're at is your net
-- worth rounded down to the nearest power of 10.
level :: Decimal -> Decimal
level = realFracToDecimal 2 . logBase 10 . realToFrac

-- | Given a level, return the net worth required to achieve that level.
unlevel :: Decimal -> Decimal
unlevel = realFracToDecimal 2 . (10**) . realToFrac

-- Shows the steps between levels
steps = let lvls = [5.0, 5.2 .. 7.0]; ls = map (realToFrac . unlevel) lvls in zip (map realToFrac lvls) (zipWith (-) (ls++[0]) (0:ls))


-- | A trivial decision is one that is between 0.01% of the total.
--
-- From <https://ofdollarsanddata.com/climbing-the-wealth-ladder/>
trivial :: Quantity
trivial = 0.0001

inUsdNow :: Maybe ValuationType
inUsdNow = Just $ AtNow $ Just "USD"

inSatNow :: Maybe ValuationType
inSatNow = Just . AtNow $ Just "SAT"

getTotal :: Journal -> Day -> ReportOpts -> String -> Quantity
getTotal j t o q = last . map aquantity $ getTotalAmounts j t o q

getTotalAmounts :: Journal -> Day -> ReportOpts -> String -> [Amount]
getTotalAmounts j d opts q = totals
  where
    opts' = opts {today_ = Just d}
    (_, (Mixed totals)) = balanceReport opts' query j
    Right (query, _) = parseQuery d $ pack q

monthlyBalance :: Journal -> Day -> Text -> BalanceReport
monthlyBalance j d q = balanceReport opts query j
  where
    opts = defreportopts {average_ = True, today_ = Just d, period_ = MonthPeriod 2020 10, interval_ = Months 6}
    Right (query, _) = parseQuery d q


-- | These are the accounts that I consider a part of my savings and not my
-- cash-spending accounts.
savingsAccounts :: [String]
savingsAccounts =
  ["as:me:save", "as:me:vest"]

-- | Savings rate is a FIRE staple. Basically take your savings and divide it by
-- your income on a monthly basis.
--
-- I think this is wronge because I need to take the monthly ammounts, but this
-- gives total amounts
savingsRate :: Journal -> Day -> Quantity
savingsRate j d = roundTo 2 $ allSavings / allIncome
  where
    allSavings = getTotal j d (defreportopts {value_ = inUsdNow}) query
    query = List.intercalate " " $ savingsAccounts
    -- gotta flip the sign because income is negative
    allIncome = - getTotal j d (defreportopts {value_ = inUsdNow}) "^in"

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
    Right (query, _) = parseQuery d $ pack "^ex"
    (_, (Mixed total)) = balanceReport opts query j
    opts =
      defreportopts
        { -- idk what the '2020 4' is for, but this actually results in the yearly
          -- report for some reason
          period_ = QuarterPeriod 2020 4,
          value_ = Just $ AtNow $ Just "USD",
          today_ = Just d
        }

-- | I have data going back to 2018.12. Use this for calculating averages per
-- month.
monthsSinceBeginning :: Day -> Quantity
monthsSinceBeginning d = fromInteger $ (year - 2019) * 12 + toInteger month + 1
  where
    (year, month, _) = toGregorian d

-- | How long until I can live off of my savings and investment returns?
--
-- Return integer is number of months until I'm free.
whenFreedom :: Journal -> Day -> Quantity
whenFreedom j d = roundTo 1 $ targetFund j d / monthlySavings
  where
    monthlySavings =
      savingsAccounts
        & map (getTotal j d (defreportopts {value_ = inUsdNow, period_ = MonthPeriod 2020 10}))
        & sum
        & \n -> (n / monthsSinceBeginning d)

-- | How many months I could sustain myself with my cash and savings, given my
-- current expenses.
runway :: Journal -> Day -> (Quantity, Quantity, Quantity)
runway j d = (nut, cash, cash / nut)
  where
    nut = (sum $ map aquantity total) / monthsSinceBeginning d
    (_, (Mixed total)) = monthlyBalance j d "^ex:me"

    cash =
      getTotal j d (defreportopts {value_ = inUsdNow}) "^as:me:save ^as:me:cash ^li:me:cred"

-- | Ramen profitability. Like 'runway', except let's say I live on /only/ the
-- necessities, and don't spend my bitcoin. So cash flow in my checking account
-- is primary.
ramen :: Journal -> Day -> (Quantity, Quantity, Quantity)
ramen j d = (nut, cash, cash / nut)
  where
    nut = (sum $ map aquantity total) / monthsSinceBeginning d
    (_, (Mixed total)) = monthlyBalance j d "^ex:me:need"
    cash = getTotal j d (defreportopts {value_ = inUsdNow}) "^as:me:cash ^li:me:cred"

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
-- v = sqrt(2 * .03 * li / net_worth)
-- v = sqrt(2 * .03 * 20,000 / 100,000)
--
-- v = 0.1095
--
-- I don't know what this means... maybe my money must be growing at an 11% rate
-- in order to cover the debt? I need to think about this equation some more.
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
