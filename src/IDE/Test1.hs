module IDE.Test1 where

import Distribution.InstalledPackageInfo
import Distribution.Package
import Distribution.Text
import GHC.Paths
import System.IO
import System.FilePath


-- getDBs = do

x :: IO ()
x = do
  fileNames <- return []
  files <- mapM readFile fileNames
  let contents = map parseInstalledPackageInfo files
  return ()

