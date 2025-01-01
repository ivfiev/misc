module Server(start) where

import Network.Socket
import Control.Monad (forever, when)
import Control.Concurrent (forkFinally)
import Network.Socket.ByteString (recv)
import Data.Functor (void)
import Data.ByteString qualified as BS
import Data.Aeson (FromJSON, decodeStrict, ToJSON)
import Blockchain
import Control.Concurrent.MVar

type Handler a = (Show a, FromJSON a, ToJSON a) => MVar (Blockchain a) -> Socket -> SockAddr -> IO ()
type Server a = (Show a, FromJSON a, ToJSON a) => Blockchain a -> Int -> IO ()

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
    void $ forkFinally (handle bcVar peerSock peerAddr) (const $ close peerSock)

handle :: forall a. Handler a
handle bcVar peerSock peerAddr = do
  bytes <- recv peerSock 4096 -- TODO msg framing/len prefix
  let len = BS.length bytes
  when (len > 0) $ do
    let block = decodeStrict bytes :: Maybe a
    maybe (return ()) (appendBlock bcVar) block
    handle bcVar peerSock peerAddr

appendBlock :: (Show a, ToJSON a) => MVar (Blockchain a) -> a -> IO ()
appendBlock bcVar block = do
  bc <- takeMVar bcVar
  let !bc' = addBlock bc block
  putMVar bcVar bc'