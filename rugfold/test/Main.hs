import Network.Socket
import Control.Monad
import Message
import System.Environment (getArgs)
import System.Process
import Text.Printf (printf)
import Utils

mkServer :: String -> String -> IO Socket
mkServer path port = do
  let addr = printf "127.0.0.1:%s" port
  void $ createProcess (shell $ printf "%s %s" path port)
  sleep 0.01
  Just s <- mbConnect addr
  return s

send :: Socket -> Message Int -> IO ()
send = sendMsg

main :: IO ()
main = do
  [path] <- getArgs
  let spawn = mkServer path
  sockets@[s1, s2, s3] <- mapM spawn ["8001", "8002", "8003"]
  send s1 $ SyncPeers ["127.0.0.1:8002", "127.0.0.1:8003"]
  send s2 $ SyncPeers ["127.0.0.1:8001", "127.0.0.1:8003"]
  send s3 $ SyncPeers ["127.0.0.1:8001", "127.0.0.1:8002"]
  let randomSocks = cycle sockets
  forM_ (zip randomSocks [1..5]) $ \(s, n) -> do
    send s $ AppendBlock n
    sleep 0.1
  send s1 DebugChain
  sleep 0.1
  send s2 DebugChain
  sleep 0.1
  send s3 DebugChain
  mapM_ close sockets
