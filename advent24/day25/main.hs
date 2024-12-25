import System.Directory.Internal.Prelude (getArgs)
import Data.List (transpose, tails)
import Control.Monad (replicateM)
import Text.Printf (printf)

main = do
  [fileName] <- getArgs
  keysLocks <- parse <$> readFile fileName
  let pairs = [(k, k') | k:keys <- tails keysLocks, k':_ <- tails keys, fit k k']
  printf "Part 1: [%d]\n" $ length pairs


parse = map transpose . splitOn null . lines where
  splitOn p [] = []
  splitOn p xs = group:splitOn p rest where
    (group, rest) = dropWhile p <$> break p xs

fit = (and .) . zipWith ((and .) . zipWith (((/= ('#', '#')) .) . (,)))