module Server(server) where

import Network.Socket
import Control.Monad
import Control.Concurrent (forkFinally)
import Data.Aeson (FromJSON, ToJSON)
import Control.Concurrent.MVar
import Blockchain
import Message
import Utils
import Data.Map (Map)
import Data.Map qualified as Map

type BC a = MVar (Blockchain a)
type Peers = MVar (Map String (Maybe Socket))

server :: (Show a, FromJSON a, ToJSON a) => Blockchain a -> String -> IO ()
server bc port = do
  bcVar <- newMVar bc
  peerVar <- newMVar Map.empty
  ai <- getAddrInfo (Just $ defaultHints { addrFlags = [AI_PASSIVE] }) Nothing (Just port)
  let addr = head ai
  sock <- socket (addrFamily addr) Stream defaultProtocol
  setSocketOption sock ReuseAddr 1
  bind sock (addrAddress addr)
  listen sock 128
  forever $ do
    (peerSock, peerAddr) <- accept sock
    forkFinally (handleConn bcVar peerVar peerSock peerAddr) (const $ close peerSock)

handleConn :: (Show a, FromJSON a, ToJSON a) => BC a -> Peers -> Socket -> SockAddr -> IO ()
handleConn bcVar peerVar peerSock peerAddr = do
  mbMsgs <- recvMsg peerSock
  case mbMsgs of
    Just msgs -> do
      mapM_ (handleMsg bcVar peerVar peerSock) msgs
      handleConn bcVar peerVar peerSock peerAddr
    Nothing -> do
      close peerSock
      return ()

handleMsg :: (Show a, ToJSON a, FromJSON a) => BC a -> Peers -> Socket -> Message a -> IO ()

handleMsg bcVar peerVar sender DebugChain = withMVar bcVar print

handleMsg bcVar peerVar sender (AppendBlock b) = do
  bc <- takeMVar bcVar
  let !bc' = addBlock bc b
  putMVar bcVar bc'
  withMVar peerVar $ \peers -> do
    let sockets = [sock | (_, Just sock) <- Map.toList peers]
    forM_ sockets $ flip sendMsg (AppendBlock b)

handleMsg bcVar peerVar sender (SyncPeers addrs) = do
  forM_ addrs $ \addr -> do
    mbSock <- mbConnect addr
    modifyMVar_ peerVar (return . Map.insert addr mbSock)

handleMsg bcVar peerVar sender (SyncHash hisHash) = do
  withMVar bcVar $ \bc -> do
    let myHash = chainHash bc
    when (hisHash /= myHash) $ do
      sendMsg sender $ SyncChain bc

handleMsg bcVar peerVar sender (SyncChain chain) = do
  bc <- takeMVar bcVar
  let newChain = if chainLength bc < chainLength chain then chain else bc
  putMVar bcVar newChain