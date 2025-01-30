module Utils where
import Control.Concurrent (newEmptyMVar, putMVar, takeMVar)
import GHC.Conc

concIO :: (a -> IO b) -> [a] -> IO [b]
concIO f xs = do
  tmp <- newEmptyMVar
  mapM_ (\x -> forkIO (f x >>= putMVar tmp)) xs
  mapM (const (takeMVar tmp)) xs