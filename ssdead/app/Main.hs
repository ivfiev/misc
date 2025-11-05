module Main where

import qualified Data.Text as Text
import SMART qualified
import Util qualified
import Calc qualified
import System.Environment
import Text.Printf

dataFile :: FilePath
dataFile = "ssdead/data"

main :: IO ()
main = do
  args <- getArgs
  point <- case args of
    [dev] -> SMART.run dev
    _ -> error "invalid args, pass '/dev/nvme<0/1/2...>'"

  csv <- Util.getState dataFile
  let points = point:[SMART.fromCsv line | line <- Text.lines csv]
  Util.putState dataFile $ Text.unlines $ map SMART.toCsv points

  let stats = Calc.stats points
  mapM_ (printf "---\n%s\n---\n" . show) stats
