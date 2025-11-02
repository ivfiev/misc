module SMART (Point(..), run, fromCsv, toCsv) where

import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Read
import qualified Util

data Point = Point
  { timestamp :: Integer,
    dataUnitsWritten :: Integer,
    powerCycles :: Integer,
    unsafeShutdowns :: Integer,
    errors :: Integer }
  deriving (Show, Eq)

run :: String -> IO Point
run dev = do
  output <- Util.runCmd ["sudo", "smartctl", "-A", dev]
  ts <- Util.timeSecs
  case output of
    Left err -> error err
    Right text -> return $ fromSmart ts text

fromSmart :: Integer -> Text -> Point
fromSmart ts text = point where

    point = Point ts written cycles shutdowns (errors1 + errors2)

    written   = getValue "Data Units Written"
    cycles    = getValue "Power Cycles"
    shutdowns = getValue "Unsafe Shutdowns"
    errors1   = getValue "Integrity Errors"
    errors2   = getValue "Error Information"

    entries = map (Text.splitOn ":") $ Text.lines text

    getValue :: Text -> Integer
    getValue key 
      | [n] <- parsed = n
      | _   <- parsed = error $ "failed to parse " ++ Text.unpack key
      where 
        parsed = [parse value | title:value:_ <- entries, key `Text.isInfixOf` title]

fromCsv :: Text -> Point
fromCsv text = mkPoint $ map parse . Text.split (== ',') . Text.strip $ text where
  
  mkPoint [ts, written, cycles, shutdowns, errors]
    = Point ts written cycles shutdowns errors

  mkPoint _ = error $ "invalid csv line: " ++ Text.unpack text

toCsv :: Point -> Text
toCsv pt = Text.intercalate "," $ Text.pack . show <$> values where
  values = [timestamp pt, dataUnitsWritten pt, powerCycles pt, unsafeShutdowns pt, errors pt]

parse :: Text -> Integer
parse = either error fst . decimal . Text.filter (/= ',') . Text.strip

