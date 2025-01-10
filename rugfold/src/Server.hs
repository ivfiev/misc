module Server(server) where

import Network.Socket
import Control.Monad
import Control.Concurrent
import Data.Aeson (FromJSON, ToJSON)
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
  void $ forkIO $ background bcVar peerVar
  forever $ do
    (peerSock, peerAddr) <- accept sock
    forkFinally (handleConn bcVar peerVar peerSock) (const $ close peerSock)

handleConn :: (Show a, FromJSON a, ToJSON a) => BC a -> Peers -> Socket -> IO ()
handleConn bcVar peerVar peerSock = do
  mbMsgs <- recvMsg peerSock
  case mbMsgs of
    Just msgs -> do
      mapM_ (handleMsg bcVar peerVar peerSock) msgs
      handleConn bcVar peerVar peerSock
    Nothing -> do
      close peerSock
      return ()

background :: forall a. (Show a, FromJSON a, ToJSON a) => BC a -> Peers -> IO ()
background bcVar peerVar = do
  sleep 0.100
  peers <- withMVar peerVar (return . connected)
  bcHash <- withMVar bcVar (return . chainHash)
  let syncMsg = SyncHash bcHash :: Message a
  mapM_ (`sendMsg` syncMsg) peers
  background bcVar peerVar

handleMsg :: (Show a, ToJSON a, FromJSON a) => BC a -> Peers -> Socket -> Message a -> IO ()

handleMsg bcVar peerVar sender DebugChain = withMVar bcVar print

handleMsg bcVar peerVar sender (AppendBlock b) = do
  bc <- takeMVar bcVar
  let !bc' = addBlock bc b
  putMVar bcVar bc'

handleMsg bcVar peerVar sender (SyncPeers addrs) = do
  forM_ addrs $ \addr -> do
    mbSock <- mbConnect addr
    modifyMVar_ peerVar (return . Map.insert addr mbSock)
    case mbSock of
      Just sock -> void $ forkFinally (handleConn bcVar peerVar sock) (const $ close sock)
      _ -> return ()

handleMsg bcVar peerVar sender (SyncHash hisHash) = do
  withMVar bcVar $ \bc -> do
    let myHash = chainHash bc
    when (hisHash /= myHash) $ do
      sendMsg sender $ SyncChain bc

handleMsg bcVar peerVar sender (SyncChain chain) = do
  bc <- takeMVar bcVar
  let newChain
        | chainLength bc < chainLength chain = chain
        | otherwise = bc
  putMVar bcVar newChain

connected :: Map String (Maybe Socket) -> [Socket]
connected peers = [sock | (_, Just sock) <- Map.toList peers]