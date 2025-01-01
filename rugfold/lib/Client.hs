module Client where

import Data.Aeson
import GHC.Generics

data Message a = Output | Add { block :: a }
  deriving (Show, Generic, FromJSON, ToJSON)