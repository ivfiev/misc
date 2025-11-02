module Calc (getStats) where

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
      "%s passed: [%.2f]\nGBs written: [%.3f]\nPower cycles: [%d]\nUnsafe shutdowns: [%d]\nErrors: [%d]" 
      timeStr timeVal gbs pwr us errs where

    (timeStr, timeVal)
      | hours < 1  = ("Minutes", hours * 60)
      | hours < 24 = ("Hours", hours)
      | otherwise  = ("Days", hours / 24)


getStats :: [Point] -> [Stat]
getStats []  = []
getStats [_] = []
getStats (now:earlier) = sortBy (flip compare `on` timeHours) stats where
  hours = [0.25, 1, 8, 24, 3 * 24, 7 * 24, 30 * 24, 90 * 24, 365 * 24]
  points = nub [minimumBy (compare `on` abs . subtract h . diff now) earlier | h <- hours]
  diff pt1 pt2 = abs $ fromIntegral (timestamp pt1 - timestamp pt2) / 3600.0
  stats = [mkStat point | point <- points]
  mkStat pt = Stat time gbs pwr us errs where
    time = diff now pt
    gbs = fromIntegral (SMART.dataUnitsWritten now - SMART.dataUnitsWritten pt) / 2048.0
    pwr = SMART.powerCycles now - SMART.powerCycles pt
    us = SMART.unsafeShutdowns now - SMART.unsafeShutdowns pt
    errs = SMART.errors now - SMART.errors pt
