import Data.Aeson
import Web.Scotty (jsonData, scotty, liftIO)
import Web.Scotty.Trans (json, post, middleware, status)
import GHC.Generics (Generic)
import Data.IORef (newIORef, readIORef, atomicModifyIORef')
import Data.Map qualified as Map
import Network.Wai (Middleware)
import Network.HTTP.Types (status404, status422)

data ApiData = ApiData
  { keywords  :: Maybe [String]
  , message   :: Maybe String
  , error     :: Maybe String
  } deriving (Generic, FromJSON, ToJSON)

mkError :: String -> ApiData
mkError msg = ApiData
  { keywords = Nothing
  , message = Nothing
  , error = Just msg }

mkResp :: [String] -> String -> ApiData
mkResp kws msg = ApiData
  { keywords = Just kws
  , message = Just msg
  , error = Nothing }

logger :: Middleware
logger app request respond = do
  putStrLn "wip req time"
  app request respond

main :: IO ()
main = do
  dbRef <- newIORef Map.empty
  scotty 8989 $ do
    middleware logger
    post "/query" $ do
      req <- jsonData
      db <- liftIO $ readIORef dbRef
      case req of
        ApiData { keywords = Just kws } -> do
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
    post "/create" $ do
      payload <- jsonData
      case payload of
        ApiData { keywords = Just kws@(_:_), message = Just msg@(_:_) } -> do
          let hash = hashKeywords kws
          let eMsg = encryptMessage kws msg
          liftIO $ atomicModifyIORef' dbRef ((,()) . Map.insert hash eMsg)
          json $ mkResp kws msg
        _ -> do
          status status422
          json $ mkError "Bad post"

hashKeywords :: [String] -> String
hashKeywords = undefined

encryptMessage :: [String] -> String -> String
encryptMessage = undefined

decryptMessage :: [String] -> String -> String
decryptMessage = undefined