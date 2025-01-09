import Network.Socket
import Control.Monad
import Control.Concurrent (threadDelay)
import Message
import System.Environment (getArgs)
import System.Process
import Text.Printf (printf)
import Utils (mbConnect)

sleep :: Double -> IO ()
sleep secs = threadDelay $ round $ secs * 1000000

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
  sockets@[s1, s2, s3] <- mapM spawn ["8989", "8998", "8899"]
  send s1 $ SyncPeers ["127.0.0.1:8998"]
  send s2 $ SyncPeers ["127.0.0.1:8899"]
  forM_ [1..3] $ \n -> do
    send s1 $ AppendBlock n
  forM_ [4..6] $ \n -> do
    send s2 $ AppendBlock n
  sleep 0.5
  send s1 DebugChain
  sleep 0.5
  send s2 DebugChain
  sleep 0.5
  send s3 DebugChain
  mapM_ close sockets
