module Server(start) where

import Network.Socket
import Control.Monad (forever, when)
import Control.Concurrent (forkFinally)
import Network.Socket.ByteString (recv)
import Data.ByteString qualified as BS
import Data.Aeson (FromJSON, decodeStrict, ToJSON)
import Control.Concurrent.MVar
import GHC.Generics (Generic)
import Blockchain

type Handler a = (Show a, FromJSON a, ToJSON a) => MVar (Blockchain a) -> Socket -> SockAddr -> IO ()
type Server a = (Show a, FromJSON a, ToJSON a) => Blockchain a -> Int -> IO ()

data Message a = Output | Add { block :: a }
  deriving (Show, Generic, FromJSON, ToJSON)

start :: Server a
start bc port = do
  bcVar <- newMVar bc
  ai <- getAddrInfo (Just $ defaultHints { addrFlags = [AI_PASSIVE] }) Nothing (Just $ show port)
  let addr = head ai
  sock <- socket (addrFamily addr) Stream defaultProtocol
  setSocketOption sock ReuseAddr 1
  bind sock (addrAddress addr)
  listen sock 128
  forever $ do
    (peerSock, peerAddr) <- accept sock
    forkFinally (handle bcVar peerSock peerAddr) (const $ close peerSock)

handle :: forall a. Handler a
handle bcVar peerSock peerAddr = do
  bytes <- recv peerSock 4096 -- TODO msg framing/len prefix
  let len = BS.length bytes
  when (len > 0) $ do
    let msg = decodeStrict bytes :: Maybe (Message a)
    case msg of
      Just Output -> output bcVar
      Just (Add block) -> appendBlock bcVar block
      _ -> return ()
    handle bcVar peerSock peerAddr

appendBlock :: (Show a, ToJSON a) => MVar (Blockchain a) -> a -> IO ()
appendBlock bcVar block = do
  bc <- takeMVar bcVar
  let !bc' = addBlock bc block
  putMVar bcVar bc'

output :: (Show a, ToJSON a) => MVar (Blockchain a) -> IO ()
output bcVar = do
  withMVar bcVar print