module Main where

import System.IO
import Http
import Datamuse
import Control.Monad
import Calc (lag)

main :: IO ()
main = mapM_ (print . lag 25 5) [30, 60, 120, 240, 360, 600, 1000]
-- main = do
--   client <- mkClient
--   forever $ do
--     putStr "> "
--     hFlush stdout
--     ws <- words <$> getLine
--     ws' <- mapM (related client . return) ws
--     mapM_ print ws'
