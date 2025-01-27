module Datamuse where
import Data.Aeson
import GHC.Generics
import Http

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
  (_, Just ml) <- client (url <> meanslike w <> "&max=100")
  (_, Just trg) <- client (url <> trigger w <> "&max=100")
  (_, Just syn) <- client (url <> synonyms w <> "&max=100")
  (_, Just ant) <- client (url <> antonyms w <> "&max=100")
  (_, Just spc) <- client (url <> kindof w <> "&max=100")
  return $ [ml, trg, syn, ant, spc] >>= map word