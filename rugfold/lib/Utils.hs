module Utils where

import Data.ByteString (ByteString)
import Data.ByteArray (convert)
import Crypto.Hash (SHA256(SHA256), hashWith, digestFromByteString, Digest)
import Data.Maybe (isJust, fromMaybe)
import Data.Word (Word8)
import Data.Char (ord)

hash :: ByteString -> ByteString
hash = convert . hashWith SHA256

maybeDigest :: ByteString -> Maybe String
maybeDigest bs = show <$> mbDigest where
  mbDigest :: Maybe (Digest SHA256)
  mbDigest = digestFromByteString bs

digest :: ByteString -> String
digest = fromMaybe (error "bad hash") . maybeDigest

isValidHash :: ByteString -> Bool
isValidHash = isJust . maybeDigest

word8 :: Char -> Word8
word8 = fromIntegral . ord