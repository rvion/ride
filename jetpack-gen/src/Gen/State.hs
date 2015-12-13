{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Gen.State
  ( getDB
  , getPackageInfos
  , listAllModuleSorted
  , packageModulesIface
  ) where

-- import Data.Aeson
import           Control.Monad
import           Data.Function                     (on)
import           Data.List                         (intercalate, isSuffixOf,
                                                    sort, sortBy)
import           Data.Map                          (Map)
import qualified Data.Map                          as M
import           Data.Text                         (Text)
import qualified Data.Text                         as T
import           Distribution.InstalledPackageInfo
import           Distribution.ModuleName
import           Distribution.Package
import           System.Directory
import           System.Directory.Tree
import           System.FilePath
import qualified System.Process                    as S
import qualified System.Process.Text               as T

import           Control.Monad.Trans.State

data State = State
  { dbs       :: [FilePath]
  , packages  :: [InstalledPackageInfo]
  , zpackages :: [InstalledPackageInfo]
  }

-- | This function returns the list of folder containing the list of installed packages
--   [ "$HOME/.stack/programs/x86_64-osx/ghc-7.10.2/lib/ghc-7.10.2/package.conf.d"
--   , "$HOME/.stack/snapshots/x86_64-osx/nightly-2015-11-29/7.10.2/pkgdb"
--   , "$HOME/dev/project/.stack-work/install/x86_64-osx/nightly-2015-11-29/7.10.2/pkgdb"
--   ]
getDB :: IO [FilePath]
getDB = do
  (exitcode, tout, terr) <- S.readProcessWithExitCode "stack"
    ["exec", "--", "ghc-pkg", "list"] ""
  let dbs = lines tout
      packages = map init $ filter (\t -> (not.null) t && head t == '/') dbs
  -- mapM print packages
  return packages

getPackageInfos :: FilePath -> IO [InstalledPackageInfo]
getPackageInfos dbpath = do
  packageFiles <- filter (isSuffixOf ".conf") <$> getDirectoryContents dbpath
  mbinfos <- mapM getPackageInfo packageFiles
  infos <- flip filterM mbinfos $ \pr -> case pr of
    ParseFailed err -> print ("error", err) >> return False
    ParseOk [] _ -> return True
    ParseOk warns _ -> print ("warning", warns) >> return True
  return $ map (\(ParseOk _ a) -> a) infos
  where getPackageInfo f = parseInstalledPackageInfo <$> readFile (dbpath </> f)

packageName :: InstalledPackageInfo -> String
packageName = unPackageName . pkgName . sourcePackageId

type ModuleNames  = [String]

packageModules :: InstalledPackageInfo -> [ModuleNames]
packageModules ipi =
   (map (components . exposedName) . exposedModules $ ipi)
   ++ map components (hiddenModules ipi)

packageModulesIface :: InstalledPackageInfo -> [(String, FilePath)]
packageModulesIface ipi =
  for (packageModules ipi) $ \mod ->
    ( intercalate "." mod
    , libdir </> (intercalate "/" mod ++ ".hi")
    )
  where
    libdir = head (libraryDirs ipi)

listAllModuleSorted :: [InstalledPackageInfo] -> IO [ExposedModule]
listAllModuleSorted ipis = do
  let
    a = sortBy (compare `on` snd) $ concatMap _modules ipis
    _modules ipis
      = map (( (\(PackageIdentifier n _) -> unPackageName n)  ( sourcePackageId ipis ),) . intercalate "." . components . exposedName) ( exposedModules ipis )
    b = concatMap exposedModules ipis
  mapM_ print a
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
    ["clone", "", "ghc-pkg", "list"] ""
  let
    lines = T.lines tout
    packages = map T.init $ filter (\t -> (not.T.null) t && T.head t == '/') lines
  mapM_ print packages
  return packages

demodb :: FilePath
demodb = "/Users/rvion/.stack/programs/x86_64-osx/ghc-7.10.2/lib/ghc-7.10.2/package.conf.d"

for = flip map

---------------------

-- So far I'll try with the string variant until text is needed.
-- Not sure about module name spec, though.

-- getDBText :: IO [Text]
-- getDBText = do
--   (exitcode, tout, terr) <- T.readProcessWithExitCode "stack"
--     ["exec", "--", "ghc-pkg", "list"] ""
--   let
--     dbs = T.lines tout
--     packages = map T.init $ filter (\t -> (not.T.null) t && (T.head t) == '/') dbs

--   mapM print packages
--   return packages

---------------------
