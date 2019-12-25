#!/usr/bin/env runhaskell
-- | Calculates and displays an overview of my finances.
module Main where
import Hledger
import Data.Either (fromRight)
import Data.Time.Calendar (Day, fromGregorian)
import Data.Text (pack)

main = do
  j <- getJournal
  let today = fromGregorian 2019 12 17
  let bal = getTotal j today
  say ["simple", show $ bal "^as:me:cash:simple status:! status:*"]
  say ["wallet", show $ bal "^as:me:cash:wallet"]
  say ["discover", show $ bal "^li:me:cred:discover status:*"]
  say ["citi", show $ bal "^li:me:cred:citi status:*"]
  say ["simple_save", show $ bal "^as:me:save:simple"]
  say ["to_save", show $ bal "^li:me:save --auto"]
  say ["btc", show $ bal "^as cur:BTC"]
  let netWorth = bal "^as ^li cur:USD"
  let netLiquid = bal "^as:me:cash ^li:me:cred cur:USD"
  let trivialWorth = 0.0001 * netWorth
  let trivialLiquid = 0.0001 * netLiquid
  say ["net_worth", show netWorth]
  say ["net_liquid", show netLiquid]
  say ["trivial_worth", show trivialWorth]
  say ["trivial_liquid", show trivialLiquid]

say = putStrLn . unwords

getTotal :: Journal -> Day -> String -> Quantity
getTotal j d q = head $ map aquantity $ netWorth
  where
    opts = defreportopts
    (query, _) = parseQuery d $ pack q
    (_, (Mixed netWorth)) = balanceReport opts query j

getJournal :: IO Journal
getJournal = do
  jp <- defaultJournalPath
  let opts = definputopts { auto_ = True }
  ej <- readJournalFile opts jp
  return $ fromRight undefined ej
