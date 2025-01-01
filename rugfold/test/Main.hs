import Network.Socket
import Network.Socket.ByteString
import Control.Monad
import Data.ByteString.Char8 qualified as BS
import Control.Concurrent (threadDelay)

main :: IO ()
main = do
  let hostname = "127.0.0.1"
      port = "8989"
  serverAddr:_ <- getAddrInfo (Just defaultHints) (Just hostname) (Just port)
  sock <- socket (addrFamily serverAddr) Stream defaultProtocol
  connect sock (addrAddress serverAddr)
  forM_ [1..10] $ \n -> do
    send sock $ BS.pack $ "{\"tag\": \"Add\", \"block\": " <> show n <> "}"
    threadDelay 1000000
  threadDelay 2000000
  send sock "{\"tag\": \"Output\"}"
  close sock
