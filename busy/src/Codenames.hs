module Codenames (runCheat) where
import Control.Monad (forever)
import System.IO
import Http
import Datamuse
import Data.Set qualified as Set
import Data.List
import Data.Function

scoreClues :: [[String]] -> [([String], [String])]
scoreClues wss = sortBy (flip compare `on` length . snd) clues where
  combinations = subsequences [(w, Set.fromList ws) | w:ws <- wss]
  clues = filter (not . null . snd) $ map score combinations
  score []  = ([], [])
  score [_] = ([], [])
  score combination = (ws, Set.toList shared) where
    shared = foldl1' Set.intersection sets
    (ws, sets) = unzip combination

runCheat :: IO ()
runCheat = do
  httpClient <- mkClient 
  forever $ do
    putStr "> "
    hFlush stdout
    ws <- words <$> getLine
    wss <- mapM (\w -> (w:) <$> related httpClient w) ws
    mapM_ (\(ws', ws'') -> putStrLn $ show ws' <> " -> " <> show ws'') $ scoreClues wss