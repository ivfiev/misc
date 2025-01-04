module Blockchain(Block, Blockchain, mkChain, addBlock) where

import Utils
import Data.ByteString qualified as BS
import Data.ByteString.Lazy (toStrict)
import Data.Aeson (ToJSON, encode, FromJSON)
import Data.List (intercalate)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import GHC.Generics (Generic)

data Block a = Block
  { body :: a
  , prevHash :: Text
  , thisHash :: Text
  , nonce :: Int }
  deriving (Generic, ToJSON, FromJSON)

data Blockchain a = Blockchain
  { blocks :: [Block a]
  , target :: Int }
  deriving (Generic, ToJSON, FromJSON)

instance Show a => Show (Block a) where
  show :: Show a => Block a -> String
  show (Block a prev curr _) = line where
    line = "[" <> show a <> ", " <> preview curr <> " <- " <> preview prev <> "]"
    preview = show . (<> "...") . Text.take 8

instance Show a => Show (Blockchain a) where
  show :: Show a => Blockchain a -> String
  show (Blockchain blocks _) = intercalate "\n" $ map show blocks

mkChain :: Int -> Blockchain a
mkChain = Blockchain []

blockHash :: ToJSON a => a -> Text -> Int -> Text
blockHash body prevHash nonce = hash $ BS.concat bss where
  bss = [Text.encodeUtf8 prevHash, toStrict $ encode nonce, toStrict $ encode body]

mineNonce :: ToJSON a => Int -> a -> Text -> Int
mineNonce target body prevHash = head $ filter goodNonce [1..] where
  prefix = Text.replicate target "0"
  goodNonce = (prefix `Text.isPrefixOf`) . blockHash body prevHash

addBlock :: (ToJSON a) => Blockchain a -> a -> Blockchain a
addBlock chain@(Blockchain blocks target) b = chain { blocks = newBlock:blocks } where
  newBlock = Block b prevHash newHash nonce
  newHash = blockHash b prevHash nonce
  prevHash
    | null blocks = Text.replicate 32 "0"
    | otherwise = thisHash $ head blocks
  !nonce = mineNonce target b prevHash