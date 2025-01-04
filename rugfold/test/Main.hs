import Network.Socket
import Control.Monad
import Control.Concurrent (threadDelay)
import Message
import System.Environment (getArgs)
import System.Process
import Text.Printf (printf)

sleep :: Double -> IO ()
sleep secs = threadDelay $ round $ secs * 1000000

mkServer :: String -> String -> IO Socket
mkServer path port = do
  let hostname = "127.0.0.1"
  void $ createProcess (shell $ printf "%s %s" path port)
  sleep 0.01
  serverAddr:_ <- getAddrInfo (Just defaultHints) (Just hostname) (Just port)
  sock <- socket (addrFamily serverAddr) Stream defaultProtocol
  connect sock (addrAddress serverAddr)
  return sock

main :: IO ()
main = do
  [path] <- getArgs
  let spawn = mkServer path
  sockets@[s1, s2] <- mapM spawn ["8989", "8990"]
  forM_ [1..50] $ \n -> do
    sendMsg s1 (Add n :: Message Int)
    sendMsg s2 (Add n :: Message Int)
  sendMsg s1 (Print :: Message Int)
  sendMsg s2 (Print :: Message Int)
  sleep 1
  mapM_ close sockets
