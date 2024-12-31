module Utils where

import Data.ByteString (ByteString)
import Data.ByteArray (convert)
import Crypto.Hash (SHA256(SHA256), hashWith, digestFromByteString, Digest)

hash :: ByteString -> ByteString
hash = convert . hashWith SHA256

digest :: ByteString -> String
digest bs = digestStr where
  digestStr = show $
    case digestFromByteString bs of
      Just d -> d :: Digest SHA256 
      Nothing -> error "bad hash"