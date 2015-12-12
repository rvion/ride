module IDE.JetpackGen where

import IDE.Iface
import IDE.State
import Data.List (isPrefixOf)
import Data.Monoid
import Control.Monad
import Data.List (intersperse)
import Data.Char
import Data.Map (Map)
import System.Directory (createDirectoryIfMissing)
import qualified Data.Map as M
import IDE.JetpackGen.Cabal (writeCabalFile)
import IDE.JetpackGen.Names
import IDE.Types

jetpackGen :: IO ()
jetpackGen = do
  dbs <- load "package DB" getDB
  packages <- load "packages" (concat <$> mapM getPackageInfos dbs)
  modules <- load "modules"  (return $ M.fromList $ concatMap packageModulesIface packages)
  reexports <- load "reexports plan" parseReexports
  deps <- load "cabal deps" parseDeps
  let f = \previouslyExportedSymbols (prefix, mod) -> do
        putStrLn ("reexport " <> mod)
        printReexports (prefix, mod, modules) previouslyExportedSymbols
  allExportsFinal <- foldM f [] reexports
  writeCabalFile reexports deps
  writeReexportModule reexports
  putStrLn "done"
  putStrLn "exported_symbols:"
  print allExportsFinal
  reexportedPackages <- return ()
  return ()

parseReexports :: IO [(String, String)]
parseReexports = do
  f <- readFile "imports.md"
  return $ map ((\(x:y:_) -> (x,y)). words . drop 4) $ filter (isPrefixOf "  - ") $ lines f

parseDeps :: IO [String]
parseDeps = do
  f <- readFile "imports.md"
  return $ map (drop 4) $ filter (isPrefixOf "  * ") $ lines f

load :: (Traversable a) => String -> IO (a b) -> IO (a b)
load str action = do
  putStr ("loading " <> str <> "... ")
  result <- action
  putStrLn ("OK (" <> show (length result) <> " elems)")
  return result

for = flip map

writeReexportModule :: [(String, String)] -> IO ()
writeReexportModule reexports = writeFile "jetpack/src/Exports.hs" content
  where
    toImport modName = concat ["\nimport ", modName, " as X"]
    content = concat $
      [ "\nmodule Exports (module X) where"
      , "\n"] ++ map (toImport.toN) reexports ++
      [ "\n\n"]

