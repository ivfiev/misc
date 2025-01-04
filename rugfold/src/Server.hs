module Server(server) where

import Network.Socket
import Control.Monad
import Control.Concurrent (forkFinally)
import Data.Aeson (FromJSON, ToJSON)
import Control.Concurrent.MVar
import Blockchain
import Message
import Utils

server :: (Show a, FromJSON a, ToJSON a) => Blockchain a -> String -> IO ()
server bc port = do
  bcVar <- newMVar bc
  peerVar <- newMVar []
  ai <- getAddrInfo (Just $ defaultHints { addrFlags = [AI_PASSIVE] }) Nothing (Just port)
  let addr = head ai
  sock <- socket (addrFamily addr) Stream defaultProtocol
  setSocketOption sock ReuseAddr 1
  bind sock (addrAddress addr)
  listen sock 128
  forever $ do
    (peerSock, peerAddr) <- accept sock
    forkFinally (handleConn bcVar peerVar peerSock peerAddr) (const $ close peerSock)

handleConn :: (Show a, FromJSON a, ToJSON a) => MVar (Blockchain a) -> MVar [Socket] -> Socket -> SockAddr -> IO ()
handleConn bcVar peerVar peerSock peerAddr = do
  mbMsgs <- recvMsg peerSock
  case mbMsgs of
    Just msgs -> do
      mapM_ (handleMsg bcVar peerVar) msgs
      handleConn bcVar peerVar peerSock peerAddr
    Nothing -> do
      close peerSock
      return ()

handleMsg :: (Show a, ToJSON a, FromJSON a) => MVar (Blockchain a) -> MVar [Socket] -> Message a -> IO ()

handleMsg bcVar peerVar Print = withMVar bcVar print

handleMsg bcVar peerVar (AppendBlock b) = do
  bc <- takeMVar bcVar
  let !bc' = addBlock bc b
  putMVar bcVar bc'
  withMVar peerVar $ \peers ->
    forM_ peers $ flip sendMsg (AppendBlock b)

handleMsg bcVar peerVar (ConnPeers addrs) = do
  forM_ addrs $ \addr -> do
    mbSock <- mbConnect addr
    case mbSock of
      Just sock -> modifyMVar_ peerVar (return . (sock:))
      Nothing -> return ()
