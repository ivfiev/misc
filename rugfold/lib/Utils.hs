module Utils where

import Data.ByteString (ByteString)
import Data.ByteArray (convert)
import Data.ByteArray.Encoding (convertToBase) -- TODO give this a go?
import Crypto.Hash (SHA256(SHA256), hashWith)
import Data.Word (Word8)
import Data.Text (Text)
import Data.Text qualified as Text
import Network.Socket 
import Control.Exception (catch)
import GHC.Exception.Type (SomeException)
import Control.Concurrent

hash :: ByteString -> Text
hash = Text.pack . show . hashWith SHA256 

word8 :: (Integral a) => Char -> a
word8 = fromIntegral . fromEnum

mbConnect :: String -> IO (Maybe Socket)
mbConnect addr = catch connection onExn where

  connection = do
    let (hostname, ':':port) = break (== ':') addr
    serverAddr:_ <- getAddrInfo (Just defaultHints) (Just hostname) (Just port)
    sock <- socket (addrFamily serverAddr) Stream defaultProtocol
    connect sock (addrAddress serverAddr)
    return $ Just sock

  onExn :: SomeException -> IO (Maybe Socket)
  onExn exn = do
    print exn
    return Nothing

sleep :: Double -> IO ()
sleep secs = threadDelay $ round $ secs * 1000000