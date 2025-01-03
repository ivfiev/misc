module Utils where

import Data.ByteString (ByteString)
import Data.ByteArray (convert)
import Data.ByteArray.Encoding (convertToBase) -- TODO give this a go?
import Crypto.Hash (SHA256(SHA256), hashWith)
import Data.Word (Word8)
import Data.Text (Text)
import Data.Text qualified as Text

hash :: ByteString -> Text
hash = Text.pack . show . hashWith SHA256 

word8 :: (Integral a) => Char -> a
word8 = fromIntegral . fromEnum