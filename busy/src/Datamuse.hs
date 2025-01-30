module Datamuse where
import Data.Aeson
import GHC.Generics
import Http
import Utils

data ApiWord = ApiWord { word :: String }
  deriving (Show, Generic, FromJSON)

url :: String
url = "https://api.datamuse.com/words"

meanslike :: String -> String
meanslike w = "?ml=" <> w

trigger :: String -> String
trigger w = "?rel_trg=" <> w

synonyms :: String -> String
synonyms w = "?rel_syn=" <> w

antonyms :: String -> String
antonyms w = "?rel_ant=" <> w

kindof :: String -> String
kindof w = "?rel_spc=" <> w

related :: HttpClient [ApiWord] -> String -> IO [String]
related client w = do
  let urls =
        [ url <> meanslike w <> "&max=100",
          url <> trigger w <> "&max=100",
          url <> synonyms w <> "&max=100",
          url <> antonyms w <> "&max=100",
          url <> kindof w <> "&max=100"  ]
  results <- concIO client urls
  return $ results >>= maybe [] (map word) . snd