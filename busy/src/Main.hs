module Main where

import System.IO
import Http
import Datamuse
import Control.Monad

main :: IO ()
main = do
  client <- mkClient
  forever $ do
    putStr "> "
    hFlush stdout
    ws <- words <$> getLine
    ws' <- mapM (related client . return) ws
    mapM_ print ws'
