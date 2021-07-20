#!/usr/bin/env runhaskell

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
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
import System.Environment (getArgs)
import qualified System.Process as Process

today :: IO Day
today = getCurrentTime >>= return . utctDay

-- | For running stuff in ghci
run :: (Journal -> Day -> a) -> IO a
run f = do
  j <- defaultJournal
  t <- today
  return $ f j t

main = do
  let janj = Just . AtNow . Just
  let banner txt = Process.callProcess "figlet" ["-f", "small", txt ]
  (cur, value_) <-
    getArgs
      >>= \case
        ["sat"] -> banner "sats" >> pure (SAT, janj "sat")
        ["sats"] -> banner "sats" >> pure (SAT, janj "sat")
        ["btc"] -> banner "bitcoin" >> pure (BTC, janj "BTC")
        _ -> banner "fiat" >> pure (USD, janj "USD")

  let reportopts = defreportopts {value_ = value_}
  j <- defaultJournal
  t <- today
  let bal = getTotal j t reportopts
  sec "cash balances"
  row "cashap" (cur $ bal "^as:me:cash:cashapp status:! status:*") Nothing
  row "wallet" (cur $ bal "^as:me:cash:wallet") Nothing
  row "   cse" (cur $ bal "^as:me:cash:cse") Nothing
  row "  disc" (cur $ bal "^li:me:cred:discover status:*") Nothing
  row "  citi" (cur $ bal "^li:me:cred:citi status:*") Nothing

  -- net cash is limited to USD because that is what I can effectively spend
  let netCash = bal "^as:me:cash ^li:me:cred cur:USD"
  let netWorth = bal "^as ^li"
  let (year, month, _) = toGregorian t
  let expectedLevel = fromJust $ Map.lookup (roundTo 2 $ (fromIntegral year + fromIntegral month / 12) - (1992 + 7 / 12)) $ levelSchedule cur
  let expectedNetWorth = unlevel expectedLevel
  let monthlyNut = nut t $ bal "^ex:me:want ^ex:me:need"
  let thisMonth = bal "^ex:me:want ^ex:me:need date:thismonth"

  sec "metrics"
  row "    in-ex" (Limit 0 $ bal "^in ^ex:me:want ^ex:me:need" / monthsSinceBeginning t) $ Just "keep this negative to make progress"
  row "    li:as" (Percent_ $ 100 * (- bal "^li") / bal "^as") Nothing
  -- let lastyear = 2020
  -- row "   in:net" (- getTotal j t (defreportopts {value_ = value_, period_ = YearPeriod 2020}) "^in:me") Nothing
  row "cred load" (Target 0 netCash) $ Just "credit spending minus cash. keep it positive"
  row "month nut" (Limit monthlyNut thisMonth) $ Just $ "avg: " <> (display $ Diff $ monthlyNut - thisMonth)
  row "      bip" (pr $ roundTo 2 $ trivial * netWorth) Nothing
  -- ideally: ramen 12 mo, runway 4 yrs.
  let (_, _, runwayMo) = runway j t reportopts
  row "   runway" (Target 12 runwayMo) $ Just "want: 12 months"
  let (ramenNut, _, ramenMo) = ramen j t reportopts
  row "    ramen" (Target 3 ramenMo) $ Just $ "want: 3 months" <> gap <> "nut: " <> display ramenNut
  let (thisyear, _, _) = toGregorian t
  let age = (fromInteger thisyear) - 1992
  let n = whenFreedom j t reportopts
  let ageFree = roundTo 1 $ (n / 12) + age :: Decimal
  row "fire rate" (Percent_ $ savingsRate j t reportopts) Nothing
  row "fire fund" (targetFund j t reportopts) Nothing
  row "when free" (Months_ n) $ Just $ "I'll be " <> pr ageFree <> " years old"

  sec "plan"
  row "net worth" (Target expectedNetWorth $ netWorth) $ Just $ "plan: " <> display (cur expectedNetWorth)
  row "    level" (Target expectedLevel $ level netWorth) $ Just $ "plan: " <> display expectedLevel
  let levelup n = level netWorth & (+ n) & roundTo' floor 1 & unlevel & \target -> target - netWorth
  let nextLevel n = Just $ display $ roundTo' floor 1 $ level netWorth + n
  row "     next" (levelup 0.1) $ nextLevel 0.1
  row "    nnext" (levelup 0.2) $ nextLevel 0.2
  row "   nnnext" (levelup 0.3) $ nextLevel 0.3
  let satbal = getTotal j t (defreportopts {value_ = janj "sat"}) "^as cur:'BTC|sat|sats'"
  row "real sats" satbal Nothing

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

-- green = achieved
-- yellow = within 5% of achieving
-- red = work to do
instance Display Metric where
  display = \case
    Target expected actual -> color actual (>=) expected $ display actual
    Limit expected actual -> color actual (<=) expected $ display actual
    where
      color :: Quantity -> (Quantity -> Quantity -> Bool) -> Quantity -> Chunk -> Chunk
      color actual cmp expected
        | actual `cmp` expected = fore green
        | actual `cmp` (expected * 0.95) = fore yellow
        | otherwise = fore red

-- | Tag numbers for different kinds of displays
data Tagged a
  = Months_ a
  | Percent_ a
  | Diff a
  | USD a
  | SAT a
  | BTC a
  | Months a

instance (Num a, Ord a, Display a) => Display (Tagged a) where
  display (USD q) = display q <> chunk " USD"
  display (SAT q) = display q <> chunk " sat"
  display (BTC q) = display q <> chunk " BTC"
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
  putChunkLn $ gap <> label <> ":" <> gap <> display value <> " " <> rest
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

levelAtAge cur age = fromJust $ Map.lookup (roundTo 2 age) $ levelSchedule cur

unlevelAtAge cur = unlevel . levelAtAge cur

-- Shows the steps between levels
steps start = zip (map realToFrac lvls) (zipWith (-) (ls ++ [0]) (0 : ls))
  where
    lvls = [start, start + 0.01 .. 8.0]
    ls = map (realToFrac . unlevel) lvls

-- | Map of age to level. Age is year + month as a decimal.
levelSchedule :: (Quantity -> Tagged Decimal) -> Map.Map Decimal Decimal
levelSchedule cur = Map.fromList $ zip ages lvls
  where
    ultimateGoal = cur 1_000_000_000 -- thats a billion usd
    (start, goal) = case ultimateGoal of
      USD n -> (5.0, level n)
      -- normalize btc/sat levels to comparative usd levels, given exchange rate
      -- assumptions below
      BTC n -> (level $ usdToBtc $ unlevel 5, level $ usdToBtc n)
      SAT n -> (level $ usdToSat $ unlevel 5, level $ usdToSat n)
    step = (goal - start) / 600
    ages = map (roundTo 2) [20, 20 + 1 / 12 .. 70]
    lvls = map (roundTo 2) [start, start + step .. goal]

-- This is a bit speculative, but I'm assuming the value of a bitcoin is
-- 100,000 USD. Eventually I should change this to actually use the USD/BTC
-- exchange rate, but it changes so much that it would be hard to have a
-- concrete plan when dealing in BTC/sats. So I figure 100k is a good price
-- target for the next year or so.
usdToBtc usd = usd / 100_000

btcToUsd btc = btc * 100_000

btcToSat btc = btc * 100_000_000

satToBtc sat = sat / 100_000_000

usdToSat usd = btcToSat $ usdToBtc usd

-- | A trivial decision is one that is 0.01% of the total, or 1 basis point.
-- From <https://ofdollarsanddata.com/climbing-the-wealth-ladder/>.
trivial :: Quantity
trivial = 0.0001

getTotal :: Journal -> Day -> ReportOpts -> String -> Quantity
getTotal j t o q = sum . map aquantity $ getTotalAmounts j t o q

getTotalAmounts :: Journal -> Day -> ReportOpts -> String -> [Amount]
getTotalAmounts j d opts q = totals
  where
    (_, (Mixed totals)) = balanceReport (ReportSpec opts d query []) j
    Right (query, _) = parseQuery d $ pack q

-- | These are the accounts that I consider a part of my savings and not my
-- cash-spending accounts.
savingsAccounts :: [String]
savingsAccounts =
  ["as:me:save", "as:me:vest"]

-- | Savings rate is a FIRE staple: (Income - Expenses) / Income * 100
savingsRate :: Journal -> Day -> ReportOpts -> Quantity
savingsRate j d opts = roundTo 2 $ 100 * (income - expenses) / income
  where
    savings = getTotal j d opts query
    query = List.intercalate " " $ savingsAccounts
    income = - getTotal j d opts "^in"
    expenses = getTotal j d opts "^ex:me:want ^ex:me:need"

-- | The target fund is simply 25x your annual expenditure.
--
-- Assumptions: 4% withdrawal rate, 3-5% return on investments.
targetFund :: Journal -> Day -> ReportOpts -> Quantity
targetFund j d opts = 25 * yearlyExpenses
  where
    yearlyExpenses = expenses / yearsSinceBeginning d
    expenses = sum $ map aquantity $ total
    Right (query, _) = parseQuery d $ pack "^ex:me:want ^ex:me:need"
    (_, (Mixed total)) = balanceReport (ReportSpec opts d query []) j

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
whenFreedom :: Journal -> Day -> ReportOpts -> Quantity
whenFreedom j d opts = roundTo 1 $ targetFund j d opts / monthlySavings j d opts

monthlySavings :: Journal -> Day -> ReportOpts -> Quantity
monthlySavings j d opts =
  savingsAccounts
    & map (getTotal j d opts)
    & sum
    & \n -> (n / monthsSinceBeginning d)

-- | How many months I could sustain myself with my cash and savings, given my
-- current expenses.
runway :: Journal -> Day -> ReportOpts -> (Quantity, Quantity, Quantity)
runway j d opts = (nut d total, cash, cash / nut d total)
  where
    total = getTotal j d opts "^ex:me:need ^ex:me:want"
    cash = getTotal j d opts "^as:me:save ^as:me:cash ^li:me:cred"

-- | Ramen profitability. Like 'runway', except let's say I live on /only/ the
-- necessities, and don't spend my bitcoin. So cash flow in my checking account
-- is primary.
ramen :: Journal -> Day -> ReportOpts -> (Quantity, Quantity, Quantity)
ramen j d opts = (nut d total, cash, cash / nut d total)
  where
    total = getTotal j d opts "^ex:me:need"
    cash = getTotal j d opts "^as:me:cash ^li:me:cred"

nut :: Day -> Quantity -> Quantity
nut d total = total / monthsSinceBeginning d
