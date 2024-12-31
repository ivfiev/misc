module Main where
import Blockchain (mkChain, addBlock, Blockchain)
import Data.List (foldl')

bc :: Blockchain Int
bc = foldl' addBlock (mkChain 0) [1, 2, 3]

main :: IO ()
main = print bc
