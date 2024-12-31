module Blockchain(Block, Blockchain, mkChain, addBlock) where

import Utils
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy (toStrict)
import Data.Aeson (ToJSON, encode)
import Data.List (intercalate, find)
import Data.Maybe (fromJust)

data Block a = Block
  { body :: a
  , prevHash :: ByteString
  , thisHash :: ByteString
  , nonce :: Int }

data Blockchain a = Blockchain 
  { blocks :: [Block a]
  , target :: Int }

instance Show a => Show (Block a) where
  show :: Show a => Block a -> String
  show (Block a prev curr _) = line where
    line = "[" <> show a <> ", " <> preview curr <> " <- " <> preview prev <> "]"
    preview = (<> "...") . take 8 . digest

instance Show a => Show (Blockchain a) where
  show :: Show a => Blockchain a -> String
  show (Blockchain blocks _) = intercalate "\n" $ map show blocks

mkChain :: Int -> Blockchain a
mkChain = Blockchain []

blockHash :: ToJSON a => a -> ByteString -> Int -> ByteString
blockHash body prevHash nonce = hash $ BS.concat bss where
  bss = [prevHash, toStrict $ encode nonce, toStrict $ encode body]

mineNonce :: ToJSON a => Int -> a -> ByteString -> Int
mineNonce target body prevHash = fromJust $ find goodNonce [1..] where
  prefix = BS.replicate target 0x0
  goodNonce = BS.isPrefixOf prefix . blockHash body prevHash

addBlock :: (ToJSON a) => Blockchain a -> a -> Blockchain a
addBlock chain@(Blockchain blocks target) b = chain { blocks = newBlock:blocks } where
  newBlock = Block b prevHash newHash nonce
  newHash = blockHash b prevHash nonce
  prevHash 
    | null blocks = hash ""
    | otherwise = thisHash $ head blocks
  !nonce = mineNonce target b prevHash