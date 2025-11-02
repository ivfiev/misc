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
    _ -> error "invalid args, pass '/dev/nvme<xyz>'"

  csv <- Util.getState dataFile
  let points = (point:) $ map SMART.fromCsv $ Text.lines csv
  Util.putState dataFile $ Text.unlines $ map SMART.toCsv points

  let stats = Calc.getStats points
  mapM_ (printf "---\n%s\n---\n" . show) stats
