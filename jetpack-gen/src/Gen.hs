module Gen where

import Control.Monad
import Data.List (isPrefixOf, intersperse, intercalate)
import Data.Monoid
import Gen.Cabal (writeCabalFile)
import Gen.Modules
import Gen.Names
import Iface
import Log
import State
import Types
import qualified Data.Map as M

jetpackGen :: IO ()
jetpackGen = do
  dbs <- trace "loading package DB" getDB
  packages <- trace "loading packages" (concat <$> mapM getPackageInfos dbs)
  modules <- trace "loading modules"  (return $ M.fromList $ concatMap packageModulesIface packages)
  reexports <- trace "loading reexports plan" parseReexports
  deps <- trace "loading cabal deps" parseDeps

  let
    reexportModule = \previouslyExportedSymbols (prefix, mod) ->
      trace ("reexporting.. " <> mod) $ do
        reexports <- findReexports (mod, modules) previouslyExportedSymbols
        printReexports (mod, prefix) reexports previouslyExportedSymbols

  allExportsFinal <- foldM reexportModule [] reexports
  writeCabalFile reexports deps
  writeReexportModule reexports
  asSuccess $ putStrLn "done"
  writeFile (jetpackFolder ++ "full-exported-symbol-list.txt") (intercalate "\n" allExportsFinal)
  let reexportedPackages = ()
  return ()

parseReexports :: IO [(String, String)]
parseReexports = do
  f <- readFile "imports.md"
  return $ map ((\(x:y:_) -> (x,y)). words . drop 4) $ filter (isPrefixOf "  - ") $ lines f

parseDeps :: IO [String]
parseDeps = do
  f <- readFile "imports.md"
  return $ map (drop 4) $ filter (isPrefixOf "  * ") $ lines f

-- IDEA (add benchmark data .)
trace :: (Traversable a) => String -> IO (a b) -> IO (a b)
trace str action = do
  asStep $ putStrLn (str <> "... ")
  result <- action
  asSuccess $ putStrLn ("  OK (" <> show (length result) <> " elems)")
  return result

for :: [a] -> (a->b) -> [b]
for = flip map

writeReexportModule :: [(String, String)] -> IO ()
writeReexportModule reexports = writeFile "jetpack/src/Exports.hs" content
  where
    toImport modName = concat ["\nimport ", modName, " as X"]
    content = concat $
      [ "\nmodule Exports (module X) where"
      , "\n"] ++ map (toImport.toN) reexports ++
      [ "\n\n"]

