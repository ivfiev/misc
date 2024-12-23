{-# LANGUAGE OverloadedLists #-}
import Data.Set qualified as S
import Data.Map qualified as M
import Data.Map ((!))
import System.Directory.Internal.Prelude (getArgs)
import Text.Printf (printf)
import Control.Monad (guard)
import Data.Foldable (maximumBy)
import Data.List (intersperse, sortBy, subsequences, sort, intercalate, tails)
import Data.Function (on)

main :: IO ()
main = do
  [fileName] <- getArgs
  graph <- parseGraph <$> readFile fileName
  let part1 = countTs $ getTriples graph
  let part2 = getPass graph
  printf "Part 1: [%d]\n" part1
  printf "Part 2: [%s]\n" part2

parseGraph :: String -> M.Map String (S.Set String)
parseGraph input = M.fromListWith (<>) pairs
  where
    pairs = do
      [a,b,'-',c,d] <- lines input
      let (pc1, pc2) = ([a,b], [c,d])
      [(pc1, [pc2]), (pc2, [pc1])]

getTriples :: M.Map String (S.Set String) -> S.Set (S.Set String)
getTriples graph = S.fromList $ do
  (pc1, net) <- M.toList graph
  pc2 <- S.toList net
  pc3 <- S.toList $ graph ! pc2
  guard $ pc3 /= pc1
  guard $ S.member pc3 net
  return [pc1, pc2, pc3]

countTs :: S.Set (S.Set String) -> Int
countTs = length . S.filter (any ((== 't') . head))

getPass :: M.Map String (S.Set String) -> String
getPass graph = intercalate "," $ sort $ maxByLen $ map maxSubset pairs 
  where
    maxByLen = maximumBy (compare `on` length)
    pairs = M.toList graph
    getSubsets (pc, pcs) = map (pc:) $ subsequences $ S.toList pcs
    maxSubset = maxByLen . filter isFullyConnected . getSubsets
    isPartConnected s = all (\s' -> s == s' || S.member s (graph ! s'))
    isFullyConnected ss = and [isPartConnected s ss' | s:ss' <- tails ss]
