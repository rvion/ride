{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Main where

import Control.Concurrent (forkIO)
import Control.Monad ()
import Control.Monad.Trans.State (StateT, evalStateT)
import qualified Control.Monad.Trans.State as S
import Control.Monad.IO.Class  (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Lens ()

import Data.Aeson (object, (.=), Value)
import Data.Monoid ((<>))

import Data.Text (Text)
import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map as M

import Network.Wai.Middleware.Static (staticPolicy, addBase)
import Network.Wai.Middleware.RequestLogger (logStdoutDev, logStdout)
import Web.Spock

import System.FilePath.Find (find, extension, (==?))
import System.IO (Handle, hPrint, hPutStrLn)
import System.Process
    ( system, ProcessHandle, createProcess, proc
    , std_out, std_in, std_err, StdStream(..)
    , terminateProcess
    )

type IDE = (Handle, Handle, Handle, ProcessHandle)
data Ctx = Ctx  { ctxIde :: Maybe IDE }
type M = StateT Ctx IO

main :: IO ()
main = runSpock 3000 $ spockT runM ride

runM :: M a -> IO a 
runM = flip evalStateT (Ctx Nothing)

ride :: SpockT M ()
ride = do
    middleware $ staticPolicy (addBase "static/")
    middleware $ logStdoutDev
    get "" $ file "test" "static/index.html"
    post "save" $ json $ object 
        [ "name" .= ("name" :: String)
        , "age" .= (3::Int)
        ]
    get "errors" $ withIde getErrors >>= json

withIde :: (IDE -> IO String) -> ActionT M String
withIde f = do
    ctx <- lift $ S.get
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
    Ctx (Just (_,_,_,procHandle)) -> do
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
haskellFiles = find (return True) (extension ==? ".hs") ""

cabalFile :: String -> FilePath
cabalFile projectName = projectName <> ".cabal"

stackFile :: FilePath
stackFile = "stack.yaml"

startGitWebUI = forkIO $ system "git webgui" >> return ()
installGitWebUI = system installGitWebUICmd
installGitWebUICmd = "curl https://raw.githubusercontent.com/alberthier/git-webui/master/install/installer.sh | bash"