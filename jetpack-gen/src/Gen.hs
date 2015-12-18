module Gen where

import           Control.Monad
import           Data.Either
import           Data.List     (intercalate, intersperse, isPrefixOf, sort)
import qualified Data.Map      as M
import           Data.Maybe
import           Data.Monoid
import           Gen.Cabal     (writeCabalFile)
import           Gen.Iface
import           Gen.Log
import           Gen.Modules
import           Gen.Names
import           Gen.State
import           Gen.Types
import           Text.Read

jetpackGen :: IO ()
jetpackGen = do
  dbs       <- trace "loading package DB" getDB
  packages  <- trace "loading packages" (concat <$> mapM getPackageInfos dbs)
  modules   <- trace "loading modules"
    (return $ M.fromList $ concatMap packageModulesIface packages)
  reexports <- trace "loading reexports plan" parseReexports
  deps      <- trace "loading cabal deps" parseDeps

  let
    reexportModule previouslyExportedSymbols (prefix, mod) =
      trace ("reexporting.. " <> mod) $ do
        reexports <- findReexports (mod, modules) previouslyExportedSymbols
        printReexports (mod, prefix) reexports previouslyExportedSymbols

  initialImport <- allexports ("BasePrelude", modules)
  -- error "done"
  allExportsFinal <- foldM reexportModule initialImport reexports
  writeCabalFile reexports deps
  writeReexportModule reexports
  asSuccess $ putStrLn "done"
  writeFile (jetpackFolder ++ "full-exported-symbol-list.txt")
    (intercalate "\n" allExportsFinal) -- it is better not to sort it
  let reexportedPackages = ()
  return ()

parseReexports :: IO [(String, String)]
parseReexports = do
  print (jetpackFolder ++ "imports.md")
  f <- readFile (jetpackFolder ++ "imports.md")
  x <- mapM myread $ lines f
  print "  parsed:"
  print $ "total: " ++ (show.length $ catMaybes x)
  return $ map (\(Qualified a b) -> (a,b)) (catMaybes x)
  where
    myread :: String -> IO (Maybe Reexport)
    myread line = do
      let mbr = readMaybe line
      case (mbr::Maybe Reexport) of
        Just r  -> putStr "  ok   : " >> asSuccess (putStrLn line)
        Nothing -> putStr "  warn : " >> asError (putStrLn line)
      return mbr

parseDeps :: IO [String]
parseDeps = do
  f <- readFile (jetpackFolder ++ "imports.md")
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
writeReexportModule reexports = writeFile (jetpackLibFolder ++ "Exports.hs") content
  where
    toImport modName = concat ["\nimport           ", modName, " as X"]
    content = concat $
      [ "\nmodule Exports (module X) where"
      , "\n"] ++  sort (map (toImport.toN) reexports) ++
      [ "\n\n"]
