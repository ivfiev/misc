module Util (runCmd, getState, putState, timeSecs) where

import Data.Text (Text) 
import Data.Text.IO qualified as TextIO
import System.Directory
import System.Environment
import System.FilePath
import Text.Printf
import System.Process
import GHC.IO.Exception
import Data.Time.Clock.POSIX

runCmd :: [String] -> IO (Either String Text)
runCmd [] = return $ Left "no cmd"
runCmd (cmd:args) = do
  let cp = (proc cmd args) {std_out = CreatePipe}
  (_, Just stdout, _, h) <- createProcess cp
  output <- TextIO.hGetContents stdout
  code <- waitForProcess h
  case code of
    ExitFailure _ -> return $ Left $ printf "failed to execute %s" cmd
    ExitSuccess   -> return $ Right output

timeSecs :: IO Integer
timeSecs = round <$> getPOSIXTime

getState :: FilePath -> IO Text
getState file = withStateFile file $ \fullPath -> do
  doesFileExist fullPath >>= \case
    False -> initState fullPath >> return ""
    True -> TextIO.readFile fullPath

putState :: FilePath -> Text -> IO ()
putState file state = withStateFile file $ flip TextIO.writeFile state

withStateFile :: FilePath -> (FilePath -> IO a) -> IO a
withStateFile filePath f = do
  home <- getEnv "HOME"
  let fullPath = printf "%s/.local/state/%s" home filePath
  f fullPath

initState :: FilePath -> IO ()
initState fullPath =
  doesFileExist fullPath >>= \case
    True -> return ()
    False -> do
      createDirectoryIfMissing True $ takeDirectory fullPath
      writeFile fullPath ""

