module Client where

import Data.Aeson
import GHC.Generics
import Network.Socket
import Network.Socket.ByteString.Lazy
import Control.Monad
import Data.ByteString.Lazy qualified as BSL

data Message a = Output | Add { block :: a }
  deriving (Show, Generic, FromJSON, ToJSON)

sendMsg :: Socket -> Message Int -> IO ()
sendMsg sock msg = send' 0 bytes where
  bytes = encode msg <> "\n"
  send' sent msg
    | BSL.length msg == sent = return ()
    | otherwise = do
      sent' <- send sock msg
      send' (sent + sent') msg