module Main where
import Blockchain (mkChain, addBlock, Blockchain)
import Data.List (foldl')
import Server
import System.Environment (getArgs)

bc :: Blockchain Int
bc = foldl' addBlock (mkChain 3) [0]

main :: IO ()
main = getArgs >>= server bc . head
