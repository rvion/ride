{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}

module Main where

import           Control.Concurrent                   (forkIO)
import           Control.Monad.IO.Class               (liftIO)
import           Control.Monad.Trans.Class            (lift)
import           Control.Monad.Trans.State            (StateT, evalStateT)
import qualified Control.Monad.Trans.State            as S

import           Data.Aeson                           (Value, object, (.=))
import           Data.Monoid                          ((<>))
import           System.Environment                   (lookupEnv)

import           Data.Map                             (Map)
import qualified Data.Map                             as M
import           Data.Text                            (Text)
import qualified Data.Text                            as T

import           Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import           Network.Wai.Middleware.Static        (addBase, staticPolicy)
import           Web.Spock

import           System.FilePath.Find                 (extension, find, (==?))
import           System.IO                            (Handle, hPrint, hPutStrLn)
import           System.Process                       (ProcessHandle, StdStream (..), createProcess, proc, std_err,
                                                       std_in, std_out, system, terminateProcess)

import           Files
-- import Yolo.Test (demo)
type IDE = (Handle, Handle, Handle, ProcessHandle)
data Ctx = Ctx  { ctxIde :: Maybe IDE }
type M = StateT Ctx IO

main :: IO ()
main =
  return ()
  -- port <- maybe 3000 read  <$> lookupEnv "PORT"
  -- runSpock port $ spockT runM ride

runM :: M a -> IO a
runM = flip evalStateT (Ctx Nothing)

ride :: SpockT M ()
ride = do
  middleware $ staticPolicy (addBase "static")
  middleware logStdoutDev
  get "files" $ liftIO fp' >>= json
  get "" $ file "test" "static/index.html"
  post "save" $ json $ object
      [ "name" .= ("name" :: String)
      , "age" .= (3::Int)
      ]
  get "errors" $ withIde getErrors >>= json

withIde :: (IDE -> IO String) -> ActionT M String
withIde f = do
  ctx <- lift S.get
  case ctxIde ctx of
    Nothing -> lift restartIde >> withIde f
    Just ide -> liftIO $ f ide

getErrors :: IDE -> IO String
getErrors (hin, hout, _, _) = do
  liftIO $ hPutStrLn hin "test"
  return "yolo"

restartIde :: M ()
restartIde = do
  prevCtx <- S.get
  case prevCtx of
    Ctx Nothing -> startIDE
    Ctx (Just (_, _, _, procHandle)) -> do
      liftIO (terminateProcess procHandle)
      startIDE

startIDE :: M ()
startIDE = do
  (Just hin, Just hout, Just herr, uid) <- liftIO $ createProcess (proc "stack" ["ide"])
     { std_out = Inherit -- Fixme
     , std_in = CreatePipe
     , std_err = CreatePipe
     }
  S.put $ Ctx $ Just (hin, hout, herr, uid)
  return ()

haskellFiles :: IO [FilePath]
haskellFiles = find (return True) (extension ==? ".hs") "src/"


cabalFile :: String -> FilePath
cabalFile projectName = projectName <> ".cabal"

stackFile :: FilePath
stackFile = "stack.yaml"

-- startGitWebUI = forkIO $ Control.Monad.void (system "git webgui")
installGitWebUI = system installGitWebUICmd
installGitWebUICmd = "curl https://raw.githubusercontent.com/alberthier/git-webui/master/install/installer.sh | bash"
