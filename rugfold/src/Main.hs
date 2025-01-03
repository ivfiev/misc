module Main where
import Blockchain (mkChain, addBlock, Blockchain)
import Data.List (foldl')
import Server

bc :: Blockchain Int
bc = foldl' addBlock (mkChain 3) [0]

main :: IO ()
main = server bc 8989
