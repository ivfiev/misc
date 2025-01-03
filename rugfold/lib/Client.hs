module Client where

import Data.Aeson
import GHC.Generics
import Network.Socket
import Network.Socket.ByteString.Lazy
import Data.ByteString.Lazy qualified as BSL
import Data.Maybe
import Utils

data Message a = Output | Add { block :: a }
  deriving (Show, Generic, FromJSON, ToJSON)

sendMsg :: (ToJSON a) => Socket -> Message a -> IO ()
sendMsg sock msg = send' 0 bytes where
  bytes = encode msg <> "\n"
  send' sent msg
    | BSL.length msg == sent = return ()
    | otherwise = do
      sent' <- send sock msg
      send' (sent + sent') msg

recvMsg :: (FromJSON a) => Socket -> IO [Message a]
recvMsg sock = recv' BSL.empty where
  recv' bs = do
    bytes <- recv sock 4096
    let bs' = bs <> bytes
    if "\n" `BSL.isSuffixOf` bs'
      then return $ mapMaybe decode $ BSL.split (word8 '\n') bs'
      else recv' bs'