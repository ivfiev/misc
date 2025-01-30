module Http where
import Network.HTTP.Types.Status
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Data.Aeson
import Control.Monad
import Text.Printf

type HttpClient a = String -> IO (Int, Maybe a)

mkClient :: forall a. (FromJSON a) => IO (HttpClient a)
mkClient = do
  manager <- newTlsManager
  return $ \url -> do
    request <- parseRequest url
    response <- httpLbs request manager
    let status = statusCode $ responseStatus response
    let mbRespBody = decode $ responseBody response
    when (status > 299) $ do
      printf "btw received HTTP status [%d]\n" status
    return (status, mbRespBody)
