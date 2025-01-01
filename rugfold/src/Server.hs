module Server(start) where

import Network.Socket
import Control.Monad (forever, unless)
import Control.Concurrent (forkFinally)
import Network.Socket.ByteString (recv)
import Data.Functor (void)
import Data.ByteString qualified as BS

start :: Int -> IO ()
start port = do
  ai <- getAddrInfo (Just $ defaultHints { addrFlags = [AI_PASSIVE] }) Nothing (Just $ show port)
  let addr = head ai
  sock <- socket (addrFamily addr) Stream defaultProtocol
  setSocketOption sock ReuseAddr 1
  bind sock (addrAddress addr)
  listen sock 128
  forever $ do
    (peerSock, peerAddr) <- accept sock
    void $ forkFinally (handle peerSock peerAddr) (const $ close peerSock)

handle :: Socket -> SockAddr -> IO ()
handle peerSock peerAddr = do
  bytes <- recv peerSock 4096
  let len = BS.length bytes
  print $ show bytes
  print len
  unless (len == 0) $ handle peerSock peerAddr
  