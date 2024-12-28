import Data.Aeson
import Web.Scotty (jsonData, scotty, liftIO)
import Web.Scotty.Trans (json, post, middleware, status)
import GHC.Generics (Generic)
import Data.IORef (newIORef, readIORef, atomicModifyIORef', IORef)
import Data.Map qualified as Map
import Network.Wai (Middleware)
import Network.HTTP.Types (status404, status422)
import Web.Scotty.Internal.Types (ActionT)
import Data.List (maximumBy, minimumBy)
import Data.Function (on)
import Crypto.Hash (hashWith, SHA256 (SHA256))
import Data.ByteString.Char8 qualified as BS
import Crypto.Cipher.Types (Cipher(cipherInit), BlockCipher (ctrCombine), makeIV)
import Crypto.Cipher.AES (AES256)
import Crypto.Error (onCryptoFailure)
import Data.Maybe (fromMaybe)

type DB = Map.Map String BS.ByteString

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

handleQuery :: IORef DB -> ActionT IO ()
handleQuery dbRef = do
  req <- jsonData
  db <- liftIO $ readIORef dbRef
  case req of
    ApiData { keywords = Just kws } -> do
      let key = hashKeywords kws
      liftIO $ putStrLn $ hashKeywords kws
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
    ApiData { keywords = Just kws@(_:_), message = Just msg@(_:_) } -> do
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
    middleware logger
    post "/query" $ handleQuery dbRef
    post "/create" $ handleCreate dbRef

hash :: String -> String
hash = show . hashWith SHA256 . BS.pack

hashKeywords :: [String] -> String
hashKeywords = hash . concat

mkAES256 :: String -> String -> BS.ByteString -> BS.ByteString
mkAES256 keyStr ivStr = ctrCombine cipher iv where
  cipher :: AES256
  cipher = onCryptoFailure (Prelude.error . show) id $ cipherInit $ mkKey 32 keyStr
  iv = fromMaybe (Prelude.error "Failed to create IV") $ makeIV $ mkKey 16 ivStr 
  mkKey len = BS.pack . take len . hash

mkMessageCipher :: [String] -> BS.ByteString -> BS.ByteString
mkMessageCipher kws = cipher where
  shortestKw = minimumBy (compare `on` length) kws
  longestKw = maximumBy (compare `on` length) kws
  cipher = mkAES256 longestKw shortestKw

encryptMessage :: [String] -> String -> BS.ByteString
encryptMessage kws = mkMessageCipher kws . BS.pack

decryptMessage :: [String] -> BS.ByteString -> String
decryptMessage kws = BS.unpack . mkMessageCipher kws