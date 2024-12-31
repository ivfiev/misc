module Main where
import Blockchain (mkChain, addBlock, Blockchain)
import Data.List (foldl')

bc :: Blockchain Int
bc = foldl' addBlock (mkChain 2) [1..10]

main :: IO ()
main = print bc
