module Server(server) where

import Network.Socket
import Control.Monad
import Control.Concurrent (forkFinally)
import Data.Aeson (FromJSON, ToJSON)
import Control.Concurrent.MVar
import Blockchain
import Message
import Data.Map (Map)
import Data.Map qualified as Map
import Utils
import Data.Maybe

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

handleConn :: (Show a, FromJSON a, ToJSON a) => MVar (Blockchain a) -> MVar (Map String Socket) -> Socket -> SockAddr -> IO ()
handleConn bcVar peerVar peerSock peerAddr = do
  mbMsgs <- recvMsg peerSock
  case mbMsgs of
    Just msgs -> do
      mapM_ (handleMsg bcVar peerVar) msgs
      handleConn bcVar peerVar peerSock peerAddr
    Nothing -> do
      close peerSock
      return ()

handleMsg :: (Show a, ToJSON a, FromJSON a) => MVar (Blockchain a) -> MVar (Map String Socket) -> Message a -> IO ()

handleMsg bcVar peerVar Print = output bcVar
handleMsg bcVar peerVar (AppendBlock b) = append bcVar b

handleMsg bcVar peerVar (ConnPeers addrs) = do
  socks <- catMaybes <$> mapM mbConnect addrs
  forM_ socks $ \peerSock -> do
    sendMsg peerSock (AppendBlock 999 :: Message Int)

handleMsg bcVar peerVar (SyncHash hash) = undefined
handleMsg bcVar peerVar (SyncBlocks blocks) = undefined

append :: (Show a, ToJSON a) => MVar (Blockchain a) -> a -> IO ()
append bcVar block = do
  bc <- takeMVar bcVar
  let !bc' = addBlock bc block
  putMVar bcVar bc'

output :: (Show a, ToJSON a) => MVar (Blockchain a) -> IO ()
output bcVar = do
  withMVar bcVar print