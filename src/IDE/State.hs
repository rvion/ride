{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}


module IDE.State where
import Data.Text (Text)
import Data.List (isSuffixOf, intercalate, sort, sortBy)
import qualified Data.Text as T
import Control.Monad
import qualified System.Process.Text as T
import qualified System.Process as S
-- import Data.Aeson
import Data.Function (on)
import Distribution.InstalledPackageInfo -- (parseInstalledPackageInfo)
import Distribution.ModuleName
import System.Directory.Tree
import System.Directory
import Data.Map (Map)
import qualified Data.Map as M
import System.FilePath
import Distribution.Package

import Control.Monad.Trans.State

data State = State
  { dbs :: [FilePath]
  , packages :: [InstalledPackageInfo]
  , zpackages :: [InstalledPackageInfo]
  }

-- "/Users/rvion/.stack/programs/x86_64-osx/ghc-7.10.2/lib/ghc-7.10.2/package.conf.d:"
-- "/Users/rvion/.stack/snapshots/x86_64-osx/nightly-2015-11-29/7.10.2/pkgdb:"
-- "/Users/rvion/dev/ride/.stack-work/install/x86_64-osx/nightly-2015-11-29/7.10.2/pkgdb:"
getDB :: IO [Text]
getDB = do
  (exitcode, tout, terr) <- T.readProcessWithExitCode "stack"
    ["exec", "--", "ghc-pkg", "list"] ""
  let
    lines = (T.lines tout)
    packages = map T.init $ filter (\t -> (not.T.null) t && (T.head t) == '/') lines

  mapM print packages
  return packages

demodb :: FilePath
demodb = "/Users/rvion/.stack/programs/x86_64-osx/ghc-7.10.2/lib/ghc-7.10.2/package.conf.d"

getPackageInfos :: FilePath -> IO [InstalledPackageInfo]
getPackageInfos dbpath = do
  packageFiles <- filter (isSuffixOf ".conf") <$> getDirectoryContents demodb
  mbinfos <- mapM getPackageInfo packageFiles
  infos <- (flip filterM) mbinfos $ \pr -> case pr of
    ParseFailed err -> print ("error", err) >> return False
    ParseOk warns a -> print ("warning", warns) >> return True
  return $ map (\(ParseOk _ a) -> a) infos
  where getPackageInfo f = parseInstalledPackageInfo <$> readFile (demodb </> f)

getModules :: IO [ExposedModule]
getModules = do
  p <- getPackageInfos ""
  let
    a = sortBy (compare `on` snd) $ concatMap _modules p
    _modules p
      = map ( (\(PackageIdentifier n _) -> unPackageName n)  (sourcePackageId p),)
      $ map (intercalate "." . components . exposedName) (exposedModules p)
    b = concatMap exposedModules p
  mapM print a
  return b


-- x :: IO ()
-- x = do
--   fileNames <- return []
--   files <- mapM readFile fileNames
--   let contents = map parseInstalledPackageInfo files
--   return ()

buildHoogle :: IO [Text]
buildHoogle = do
  (exitcode, tout, terr) <- T.readProcessWithExitCode "git"
    (["clone", "", "ghc-pkg", "list"]) ""
  let
    lines = (T.lines tout)
    packages = map T.init $ filter (\t -> (not.T.null) t && (T.head t) == '/') lines

  mapM print packages
  return packages
