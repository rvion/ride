module IDE.JetpackGen where

import IDE.Iface
import IDE.State
import Data.List (isPrefixOf)
import Data.Monoid

import Data.Map (Map)
import qualified Data.Map as M

jetpackGen :: IO ()
jetpackGen = do
  dbs       <- load "package DB" getDB
  packages  <- load "packages" (concat <$> mapM getPackageInfos dbs)
  modules   <- load "modules"  (return $ M.fromList $ concatMap packageModulesIface packages)
  reexports <- load "reexports plan" (M.fromList <$> parseReexports)
  print "done"
  reexportedPackages <- return ()
  return ()

parseReexports :: IO [(String, String)]
parseReexports = do
  f <- readFile "imports.md"
  let reexports = map ((\(x:y:_) -> (x,y)). words . drop 4) $ filter (isPrefixOf "  - ") $ lines f
  return reexports

load :: (Traversable a) => String -> IO (a b) -> IO (a b)
load str action = do
  putStr ("loading " <> str <> "... ")
  result <- action
  putStrLn ("OK (" <> show (length result) <> " elems)")
  return result

