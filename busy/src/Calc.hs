module Calc where

lag :: Double -> Double -> Double -> Double
lag ping rr fps = (1000.0 / fps + rr) / 2 + ping