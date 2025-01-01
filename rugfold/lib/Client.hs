module Client where

import Data.Aeson
import GHC.Generics
import Network.Socket
import Network.Socket.ByteString.Lazy
import Control.Monad

data Message a = Output | Add { block :: a }
  deriving (Show, Generic, FromJSON, ToJSON)

sendMsg :: Socket -> Message Int -> IO ()
sendMsg sock msg = void $ send sock $ encode msg  -- just a single 'send' - assumes entire message fits