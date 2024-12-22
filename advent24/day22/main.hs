import Control.Monad (replicateM)
import Data.Bits (Bits (xor))
import System.Directory.Internal.Prelude (getArgs)
import Text.Printf (printf)

main :: IO ()
main = do
  [inputFile] <- getArgs
  initialSecretNumbers <- map read . lines <$> readFile inputFile
  let allSecretNumbers = map (iterate nextSecretNumber) initialSecretNumbers
  let part1 = sum $ (!! 2000) <$> allSecretNumbers
  printf "Part 1: [%d]\n" part1

nextSecretNumber :: Integer -> Integer
nextSecretNumber secret = secret'
  where
    mix = xor
    prune = flip rem 16777216
    secret' = foldr (\f s -> prune $ mix s $ f s) secret [(* 2048), (`div` 32), (* 64)]