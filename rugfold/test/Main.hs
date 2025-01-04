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
  sockets@[s1, s2] <- mapM spawn ["8989", "8990"]
  send s1 $ ConnPeers ["127.0.0.1:8990"]
  forM_ [1..2] $ \n -> do
    send s1 $ AppendBlock n
  sleep 0.5
  send s2 Print
  sleep 0.1
  send s1 Print
  sleep 0.1
  mapM_ close sockets
