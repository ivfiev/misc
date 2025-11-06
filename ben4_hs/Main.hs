import Control.Monad.ST (runST)
import Data.Int (Int32)
import Control.Monad
import Data.STRef
import GHC.Arr (newSTArray)
import GHC.IOArray
import Data.IORef
import Control.Concurrent
import System.Posix (sleep)
import Text.Printf


main = do
  count <- newIORef 0
  primes <- newIOArray (0, 1229) (0 :: Int)
  finished <- newIORef False
  forkIO $ do
    threadDelay (1 * 1000000)
    writeIORef finished True
  work primes count finished
  c <- readIORef count
  print c

work :: IOArray Int Int -> IORef Int -> IORef Bool -> IO ()
work primes count finished = do
  fin <- readIORef finished
  unless fin $ do
    forM_ [0..1228] $ \i -> writeIOArray primes i 0
    k <- newIORef 1
    writeIOArray primes 0 2
    forM_ [3, 5..10000] $ \n -> do
      k' <- readIORef k
      inner 0 k' n primes k
    modifyIORef' count (+1)
    work primes count finished

inner i lim n primes k = do
  unless (i == lim) $ do
    p <- readIOArray primes i
    case rem n p of
      0 -> return ()
      _ | n < p^2 -> writeIOArray primes lim n >> modifyIORef' k (+1)
      _ -> inner (i + 1) lim n primes k
