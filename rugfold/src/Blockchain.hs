module Blockchain(Block, Blockchain, mkChain, addBlock) where

import Utils
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy (toStrict)
import Data.Aeson (ToJSON, encode)
import Data.List (intercalate)

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
  bss = [toStrict $ encode body, prevHash, toStrict $ encode nonce]

addBlock :: ToJSON a => Blockchain a -> a -> Blockchain a
addBlock (Blockchain [] target) b = Blockchain [genesisBlock] target where
  genesisBlock = Block b prevHash (blockHash b prevHash target) 0
  prevHash = hash ""
addBlock chain@(Blockchain blocks@(prevBlock:_) _) b = chain { blocks = newBlock:blocks } where
  newBlock = Block b prevHash newHash nonce
  newHash = blockHash b prevHash nonce
  prevHash = thisHash prevBlock
  nonce = 0
  