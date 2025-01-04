module Server(server) where

import Network.Socket
import Control.Monad (forever)
import Control.Concurrent (forkFinally)
import Data.Aeson (FromJSON, ToJSON)
import Control.Concurrent.MVar
import Blockchain
import Message

server :: (Show a, FromJSON a, ToJSON a) => Blockchain a -> String -> IO ()
server bc port = do
  bcVar <- newMVar bc
  ai <- getAddrInfo (Just $ defaultHints { addrFlags = [AI_PASSIVE] }) Nothing (Just port)
  let addr = head ai
  sock <- socket (addrFamily addr) Stream defaultProtocol
  setSocketOption sock ReuseAddr 1
  bind sock (addrAddress addr)
  listen sock 128
  forever $ do
    (peerSock, peerAddr) <- accept sock
    forkFinally (handleConn bcVar peerSock peerAddr) (const $ close peerSock)

handleConn :: (Show a, FromJSON a, ToJSON a) => MVar (Blockchain a) -> Socket -> SockAddr -> IO ()
handleConn bcVar peerSock peerAddr = do
  mbMsgs <- recvMsg peerSock
  case mbMsgs of
    Just msgs -> do
      mapM_ (handleMsg bcVar) msgs
      handleConn bcVar peerSock peerAddr
    Nothing -> do
      close peerSock
      return ()

handleMsg :: (Show a, ToJSON a, FromJSON a) => MVar (Blockchain a) -> Message a -> IO ()
handleMsg bcVar Print = output bcVar
handleMsg bcVar (Add block) = append bcVar block
handleMsg bcVar (SyncHash hash) = undefined
handleMsg bcVar (SyncBlocks blocks) = undefined

append :: (Show a, ToJSON a) => MVar (Blockchain a) -> a -> IO ()
append bcVar block = do
  bc <- takeMVar bcVar
  let !bc' = addBlock bc block
  putMVar bcVar bc'

output :: (Show a, ToJSON a) => MVar (Blockchain a) -> IO ()
output bcVar = do
  withMVar bcVar print