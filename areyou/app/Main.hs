import Crypto
import Data.Aeson (FromJSON, ToJSON)
import Web.Scotty (jsonData, scotty, liftIO)
import Web.Scotty.Trans (json, post, middleware, status)
import GHC.Generics (Generic)
import Data.IORef (newIORef, readIORef, atomicModifyIORef', IORef)
import Data.Map (Map)
import Data.Map qualified as Map
import Network.Wai (Middleware)
import Network.HTTP.Types (status404, status422)
import Web.Scotty.Internal.Types (ActionT)
import Data.ByteString.Char8 (ByteString)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Text.Printf (printf)
import Control.Exception (bracket)
import Data.Text (Text)

type DB = Map ByteString ByteString

data ApiData = ApiData
  { keywords  :: Maybe [Text]
  , message   :: Maybe Text
  , error     :: Maybe Text
  } deriving (Generic, FromJSON, ToJSON)

mkError :: Text -> ApiData
mkError msg = ApiData
  { keywords = Nothing
  , message = Nothing
  , error = Just msg }

mkResp :: [Text] -> Text -> ApiData
mkResp kws msg = ApiData
  { keywords = Just kws
  , message = Just msg
  , error = Nothing }

logger :: IORef DB -> Middleware
logger dbRef app request respond =
  bracket getPOSIXTime
    (\us0 -> do
      us1 <- getPOSIXTime
      db <- readIORef dbRef
      let elapsedUs = round $ 1000000 * (us1 - us0) :: Int
      printf "[%s]\nElapsed us: [%d]\nDB: [%s]\n" (show request) elapsedUs (show db))
    (\_ -> app request respond)

handleQuery :: IORef DB -> ActionT IO ()
handleQuery dbRef = do
  req <- jsonData
  db <- liftIO $ readIORef dbRef
  case req of
    ApiData { keywords = Just kws@(_:_) } -> do
      let key = hashKeywords kws
      case Map.lookup key db of
        Nothing -> do
          status status404
          json $ mkError "No message"
        Just eMsg -> do
          json $ mkResp kws $ decryptMessage kws eMsg
    _ -> do
      status status422
      json $ mkError "Bad query"

handleCreate :: IORef DB -> ActionT IO ()
handleCreate dbRef = do
  payload <- jsonData
  case payload of
    ApiData { keywords = Just kws@(_:_), message = Just msg } -> do
      let key = hashKeywords kws
      let encrypted = encryptMessage kws msg
      liftIO $ atomicModifyIORef' dbRef ((,()) . Map.insert key encrypted)
      json $ mkResp kws msg
    _ -> do
      status status422
      json $ mkError "Bad post"

main :: IO ()
main = do
  dbRef <- newIORef Map.empty
  scotty 8989 $ do
    middleware $ logger dbRef
    post "/query" $ handleQuery dbRef
    post "/create" $ handleCreate dbRef
