#!/usr/bin/env runhaskell

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | Calculates and displays an overview of my finances.
module Main where

import Data.Decimal (Decimal (..), DecimalRaw (..), divide, realFracToDecimal, roundTo, roundTo')
import Data.Either (fromRight)
import Data.Function ((&))
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Text (Text, pack)
import qualified Data.Text as T
import qualified Data.Text.IO as IO
import Data.Time.Calendar (Day, fromGregorian, toGregorian)
import Data.Time.Clock (UTCTime (..), diffTimeToPicoseconds, diffUTCTime, getCurrentTime)
import Hledger
import Rainbow

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
  row "simple" (bal "^as:me:cash:simple status:! status:*") Nothing
  row "cashap" (bal "^as:me:cash:cashapp status:! status:*") Nothing
  row "wallet" (bal "^as:me:cash:wallet") Nothing
  row "  disc" (bal "^li:me:cred:discover status:*") Nothing
  row "  citi" (bal "^li:me:cred:citi status:*") Nothing
  let btcBal q = sum . map aquantity $ getTotalAmounts j t defreportopts q
  let btcBalUSD q = sum . map aquantity $ getTotalAmounts j t (defreportopts {value_ = inUsdNow}) q
  row "   btc" (Target 4 $ btcBal "^as cur:BTC") $ Just $ display $ btcBalUSD "^as cur:BTC"

  let netCash = bal "^as:me:cash ^li:me:cred cur:USD"
  let netWorth = balVal "^as ^li"
  let (year, month, _) = toGregorian t
  let expectedLevel = fromJust $ Map.lookup (roundTo 2 $ (fromIntegral year + fromIntegral month / 12) - (1992 + 7 / 12)) levelSchedule
  let expectedNetWorth = unlevel expectedLevel
  let monthlyNut = nut t $ balVal "^ex"
  let thisMonth = balVal "^ex date:thismonth"

  sec "metrics"
  row "  in - ex" (Limit 0 $ bal "^in ^ex" / monthsSinceBeginning t) $ Just "keep this negative to make progress"
  row "cred load" (Target 0 netCash) $ Just "credit spending minus cash. keep it positive"
  row "month exp" (Limit monthlyNut thisMonth) $ Just $ "avg: " <> (display $ Diff $ monthlyNut - thisMonth)

  sec "plan"
  row "net worth" (Target expectedNetWorth $ netWorth) $ Just $ "plan: " <> display expectedNetWorth
  row "    level" (Target expectedLevel $ level netWorth) $ Just $ "plan: " <> display expectedLevel
  let levelup n = level netWorth & (+ n) & roundTo' floor 1 & unlevel & \target -> target - netWorth
  row "     next" (levelup 0.1) $ Just $ display $ roundTo' floor 1 $ level netWorth + 0.1
  row "    nnext" (levelup 0.2) $ Just $ display $ roundTo' floor 1 $ level netWorth + 0.2
  row "   nnnext" (levelup 0.3) $ Just $ display $ roundTo' floor 1 $ level netWorth + 0.3

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
  row "savings rate" (Percent_ $ savingsRate j t) Nothing
  row " target fund" (targetFund j t) Nothing
  row "   when free" (Months_ n) $ Just $ "I'll be " <> pr ageFree <> " years old"

  sec "runway"
  let (nut, cash, months) = runway j t
  row "   nut" nut Nothing
  row "  cash" cash Nothing
  row "months" (Target 36 months) $ Just "want: 3 years"

  sec "ramen"
  let (nut, cash, months) = ramen j t
  row "   nut" nut Nothing
  row "  cash" cash Nothing
  row "months" (Target 3 months) $ Just "want: 3 months"

-- | <type> <expected> <actual>
data Metric
  = Target Quantity Quantity
  | Limit Quantity Quantity

-- | Tag displayable things so I can change how things print, e.g. add a percent
-- sign, or some coloring, etc.
class Display a where
  display :: a -> Chunk

instance Display Chunk where
  display c = c

instance Display Metric where
  display (Target expected actual) = color $ display actual
    where
      color = if actual >= expected then fore green else fore red
  display (Limit expected actual) = color $ display actual
    where
      color = if actual <= expected then fore green else fore red

-- | Tag numbers for different kinds of displays
data Number
  = Months_ Quantity
  | Percent_ Quantity
  | Diff Quantity

instance Display Number where
  display (Months_ q) = display q <> " months"
  display (Percent_ p) = display p <> "%"
  display (Diff n)
    | n > 0 = "+" <> display n
    | n < 0 = "-" <> display n
    | n == 0 = "=="

instance Display Text where
  display t = chunk t

-- | Pretty-print a number. From https://stackoverflow.com/a/61070523/1146898
instance Display Quantity where
  display d = chunk $
    T.intercalate "." $ case T.splitOn "." $ T.pack $ show $ roundTo 2 d of
      x : xs -> (T.pack . reverse . go . reverse . T.unpack) x : xs
      xs -> xs
    where
      go (x : y : z : []) = x : y : z : []
      go (x : y : z : ['-']) = x : y : z : ['-']
      go (x : y : z : xs) = x : y : z : ',' : go xs
      go xs = xs

sec :: String -> IO ()
sec label = putStrLn $ "\n" <> label <> ":"

pr :: Show s => s -> Chunk
pr = chunk . pack . show

row :: (Display a) => Chunk -> a -> Maybe Chunk -> IO ()
row label value note =
  putChunkLn $ gap <> label <> ":" <> gap <> display value <> rest
  where
    rest = case note of
      Nothing -> ""
      Just a -> gap <> "\t(" <> a <> ")"

gap :: Chunk
gap = "  "

-- | There's levels to life, a proxy metric for what level you're at is your net
-- worth rounded down to the nearest power of 10.
level :: Decimal -> Decimal
level = realFracToDecimal 2 . logBase 10 . realToFrac

-- | Given a level, return the net worth required to achieve that level.
unlevel :: Decimal -> Decimal
unlevel = realFracToDecimal 2 . (10 **) . realToFrac

-- Shows the steps between levels
steps start = zip (map realToFrac lvls) (zipWith (-) (ls ++ [0]) (0 : ls))
  where
    lvls = [start, start + 0.01 .. 8.0]
    ls = map (realToFrac . unlevel) lvls

levelSchedule = Map.fromList $ zip ages lvls
  where
    start = 5.0
    goal = 9.0
    step = (goal - start) / 600
    ages = map (roundTo 2) [20, 20 + 1 / 12 .. 70]
    lvls = map (roundTo 2) [start, start + step .. goal]

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

-- | These are the accounts that I consider a part of my savings and not my
-- cash-spending accounts.
savingsAccounts :: [String]
savingsAccounts =
  ["as:me:save", "as:me:vest"]

-- | Savings rate is a FIRE staple: (Income - Expenses) / Income * 100
savingsRate :: Journal -> Day -> Quantity
savingsRate j d = roundTo 2 $ 100 * (income - expenses) / income
  where
    -- I used to do just savings/income, but this is wrong because it also
    -- includes capital gains, which are not technically part of the savings rate.
    --roundTo 2 $ savings / income

    opts = defreportopts {value_ = inUsdNow}
    savings = getTotal j d opts query
    query = List.intercalate " " $ savingsAccounts
    -- gotta flip the sign because income is negative
    income = - getTotal j d opts "^in"
    expenses = getTotal j d opts "^ex"

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
    yearlyExpenses = expenses / yearsSinceBeginning d
    expenses = sum $ map aquantity $ total
    Right (query, _) = parseQuery d $ pack "^ex"
    (_, (Mixed total)) = balanceReport opts query j
    opts =
      defreportopts
        { value_ = inUsdNow,
          today_ = Just d
        }

-- | I have expense data going back to 2019.10. Use this for calculating
-- averages per month.
monthsSinceBeginning :: Day -> Quantity
monthsSinceBeginning d =
  diffUTCTime (UTCTime d 0) start
    & secondsToMonths
    & mkDecimal
  where
    mkDecimal n = fromRational $ toRational n :: Decimal
    secondsToMonths s = s / 60 / 60 / 24 / 7 / 4
    start = UTCTime (fromGregorian 2019 10 1) 0

yearsSinceBeginning :: Day -> Quantity
yearsSinceBeginning d = monthsSinceBeginning d / 12

-- | How long until I can live off of my savings and investment returns?
--
-- Return integer is number of months until I'm free.
whenFreedom :: Journal -> Day -> Quantity
whenFreedom j d = roundTo 1 $ targetFund j d / monthlySavings j d

monthlySavings :: Journal -> Day -> Quantity
monthlySavings j d =
  savingsAccounts
    & map (getTotal j d (defreportopts {value_ = inUsdNow}))
    & sum
    & \n -> (n / monthsSinceBeginning d)

-- | How many months I could sustain myself with my cash and savings, given my
-- current expenses.
runway :: Journal -> Day -> (Quantity, Quantity, Quantity)
runway j d = (nut d total, cash, cash / nut d total)
  where
    opts = defreportopts {value_ = inUsdNow}
    total = getTotal j d opts "^ex:me"
    cash = getTotal j d opts "^as:me:save ^as:me:cash ^li:me:cred"

-- | Ramen profitability. Like 'runway', except let's say I live on /only/ the
-- necessities, and don't spend my bitcoin. So cash flow in my checking account
-- is primary.
ramen :: Journal -> Day -> (Quantity, Quantity, Quantity)
ramen j d = (nut d total, cash, cash / nut d total)
  where
    opts = defreportopts {value_ = inUsdNow}
    total = getTotal j d opts "^ex:me:need"
    cash = getTotal j d opts "^as:me:cash ^li:me:cred"

nut :: Day -> Quantity -> Quantity
nut d total = total / monthsSinceBeginning d

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
