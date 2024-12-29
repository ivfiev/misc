module Crypto(hashKeywords, encryptMessage, decryptMessage) where

import Crypto.Hash (hashWith, SHA256 (SHA256))
import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Char8 qualified as BS
import Crypto.Cipher.Types (Cipher(cipherInit), BlockCipher (ctrCombine), makeIV)
import Crypto.Cipher.AES (AES256)
import Crypto.Error (onCryptoFailure)
import Data.Maybe (fromMaybe)
import Data.List (maximumBy, minimumBy)
import Data.Function (on)
import Data.ByteArray (convert)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8, decodeUtf8Lenient)

hash :: ByteString -> ByteString
hash = convert . hashWith SHA256

hashKeywords :: [Text] -> ByteString
hashKeywords = hash . BS.concat . map encodeUtf8

mkAES256 :: ByteString -> ByteString -> ByteString -> ByteString
mkAES256 keyStr ivStr = ctrCombine cipher iv where
  cipher :: AES256
  cipher = onCryptoFailure (Prelude.error . show) id $ cipherInit $ mkKey 32 keyStr
  iv = fromMaybe (Prelude.error "Failed to create IV") $ makeIV $ mkKey 16 ivStr
  mkKey len = BS.take len . hash

mkMessageCipher :: [ByteString] -> ByteString -> ByteString
mkMessageCipher kws = cipher where
  shortestKw = minimumBy (compare `on` BS.length) kws
  longestKw = maximumBy (compare `on` BS.length) kws
  cipher = mkAES256 longestKw shortestKw

encryptMessage :: [Text] -> Text -> ByteString
encryptMessage kws = mkMessageCipher (map encodeUtf8 kws) . encodeUtf8

decryptMessage :: [Text] -> ByteString -> Text
decryptMessage kws = decodeUtf8Lenient . mkMessageCipher (map encodeUtf8 kws)