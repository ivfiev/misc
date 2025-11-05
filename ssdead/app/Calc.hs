module Calc (stats) where

import SMART (Point (..), timestamp)
import Text.Printf
import Data.Function
import Data.List

data Stat = Stat 
  { timeHours :: Double, 
    gbsWritten :: Double, 
    powerCycles :: Integer, 
    unsafeShutdowns :: Integer, 
    errors :: Integer }

instance Show Stat where
  show (Stat hours gbs pwr us errs) = 
    printf 
      "%s\nGBs written: [%.3f]\nPower cycles: [%d]\nUnsafe shutdowns: [%d]\nErrors: [%d]" 
      timeStr gbs pwr us errs where

    timeStr :: String
    timeStr
      | hours < 1  = printf "Minutes passed: [%d]" (round (hours * 60) :: Int)
      | hours < 24 = printf "Hours passed: [%.2f]" hours
      | otherwise  = printf "Days passed: [%.2f]" (hours / 24)


stats :: [Point] -> [Stat]
stats []  = []
stats [_] = []
stats (now:earlier) = sortBy (flip compare `on` timeHours) [mkStat point | point <- points] where

  points = nub [minimumBy (compare `on` abs . subtract h . diffHours now) earlier | h <- hours]

  hours :: [Double]
  hours = [0.25, 1, 8, 24, 3 * 24, 7 * 24, 30 * 24, 90 * 24, 365 * 24]

  diffHours pt1 pt2 = abs $ fromIntegral (timestamp pt1 - timestamp pt2) / 3600.0

  mkStat pt = Stat time gbs pwr us errs where
    time = diffHours now pt
    gbs = fromIntegral (SMART.dataUnitsWritten now - SMART.dataUnitsWritten pt) / 2048.0
    pwr = SMART.powerCycles now - SMART.powerCycles pt
    us = SMART.unsafeShutdowns now - SMART.unsafeShutdowns pt
    errs = SMART.errors now - SMART.errors pt
