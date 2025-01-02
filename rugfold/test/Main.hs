import Network.Socket
import Control.Monad
import Control.Concurrent (threadDelay)
import Client

sleep :: Double -> IO ()
sleep secs = threadDelay $ round $ secs * 1000000

main :: IO ()
main = do
  let hostname = "127.0.0.1"
      port = "8989"
  serverAddr:_ <- getAddrInfo (Just defaultHints) (Just hostname) (Just port)
  sock <- socket (addrFamily serverAddr) Stream defaultProtocol
  connect sock (addrAddress serverAddr)
  forM_ [1..10] $ \n -> do
    sendMsg sock $ Add n
  sendMsg sock Output
  sleep 0.5
  close sock
