module Datamuse where
import Data.List
import Data.Aeson
import GHC.Generics
import Http

data ApiWord = ApiWord { word :: String }
  deriving (Show, Generic, FromJSON)

url :: String
url = "https://api.datamuse.com/words"

meanslike :: [String] -> String
meanslike ws = "?ml=" <> intercalate "+" ws

related :: HttpClient [ApiWord] -> [String] -> IO [String]
related client ws = do
  (_, Just ws') <- client (url <> meanslike ws <> "&max=10")
  return $ map word ws'