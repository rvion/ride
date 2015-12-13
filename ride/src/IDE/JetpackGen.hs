module IDE.JetpackGen where

import System.Console.ANSI
import Control.Monad
import Data.List (isPrefixOf)
import Data.Monoid
import IDE.Iface
import IDE.JetpackGen.Cabal (writeCabalFile)
import IDE.JetpackGen.Modules
import IDE.JetpackGen.Names
import IDE.State
import qualified Data.Map as M
import IDE.Types
import Data.List (intersperse)

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
  writeFile (jetpackFolder ++ "allExportsFinal.txt") (concat $ intersperse "\n" allExportsFinal)
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

-- TODO (add benchmark data)
trace :: (Traversable a) => String -> IO (a b) -> IO (a b)
trace str action = do
  asStep $ putStrLn (str <> "... ")
  result <- action
  asSuccess $ putStrLn ("  OK (" <> show (length result) <> " elems)")
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


asStep, asSuccess, asInfo, asWarning, asError :: IO a -> IO a
asStep = writeIn Blue
asSuccess = writeIn Green
asInfo = writeIn Cyan
asWarning = writeIn Yellow
asError = writeIn Red

writeIn :: Color -> IO a -> IO a
writeIn col x = do
  setSGR [SetColor Foreground Vivid col]
  _r <- x
  setSGR [Reset]
  return _r

