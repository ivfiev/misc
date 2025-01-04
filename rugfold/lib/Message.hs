module Message where

import Data.Aeson
import GHC.Generics
import Network.Socket
import Network.Socket.ByteString.Lazy
import Data.ByteString.Lazy qualified as BSL
import Data.Maybe
import Utils
import Control.Exception
import Data.Text (Text)
import Blockchain (Block)

data Message a = 
  Print 
  | AppendBlock a
  | ConnPeers [String]
  | SyncHash Text 
  | SyncBlocks [Block a]
  deriving (Show, Generic, FromJSON, ToJSON)

sendMsg :: (ToJSON a) => Socket -> Message a -> IO ()
sendMsg sock msg = catch (sendAll sock bytes) (onSockExn sock) where
  bytes = encode msg <> "\n"

recvMsg :: (FromJSON a) => Socket -> IO (Maybe [Message a])
recvMsg sock = catch (recv' "") (const (return Nothing) . onSockExn sock) where
  recv' bs = do
    r <- recv sock 4096
    let bs' = bs <> r
    case r of
      "" -> return Nothing
      _ | "\n" `BSL.isSuffixOf` bs' -> 
        return $ Just $ mapMaybe decode $ BSL.split (word8 '\n') bs'
      _ -> recv' bs'

onSockExn :: Socket -> SomeException -> IO ()
onSockExn sock = print