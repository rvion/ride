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

jetpackGen :: IO ()
jetpackGen = do
  dbs <- load "package DB" getDB
  packages <- load "packages" (concat <$> mapM getPackageInfos dbs)
  modules <- load "modules"  (return $ M.fromList $ concatMap packageModulesIface packages)
  reexports <- load "reexports plan" parseReexports
  deps <- load "cabal deps" parseDeps
  let f = \previouslyExportedSymbols (prefix, mod) -> do
        putStrLn ("reexport " <> mod)
        printReexports (prefix, modules M.! mod) previouslyExportedSymbols
  foldM_ f [] reexports
  writeCabalFile reexports deps
  writeReexportModule reexports
  print "done"
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

writeCabalFile :: [(String, String)] -> [String] -> IO ()
writeCabalFile reexports deps = writeFile "jetpack/jetpack.cabal" content
  where
    content = concat $
      [ "\nname:                jetpack"
      , "\nversion:             0.1.0.0"
      , "\nsynopsis:            Initial project template from stack"
      , "\ndescription:         Please see README.md"
      , "\nhomepage:            http://github.com/githubuser/jetpack#readme"
      , "\nlicense:             BSD3"
      , "\nauthor:              Rémi Vion"
      , "\nmaintainer:          vion.remi@gmail.com"
      , "\ncopyright:           2016 Author Here"
      , "\ncategory:            Prelude"
      , "\nbuild-type:          Simple"
      , "\ncabal-version:       >=1.10"
      , "\n"
      , "\nlibrary"
      , "\n  hs-source-dirs:      src"
      , "\n  exposed-modules:     JetPack"
      , "\n  other-modules:       Exports, "]
      ++ intersperse ", " (map toN reexports) ++
      [ "\n  build-depends:       "] ++ intersperse ", " deps ++
      [ "\n  default-language:    Haskell2010"
      , "\n"
      , "\nsource-repository head"
      , "\n  type:     git"
      , "\n  location: https://github.com/rvion/jetpack"
      ]

toN (prefix, mod) = concat [mod, ".", "As", _typePrefix]
  where
    (p:ps) = prefix
    _idPrefix = (toLower p : ps)
    _typePrefix = (toUpper p : ps)


