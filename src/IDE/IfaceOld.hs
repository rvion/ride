{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BangPatterns #-}

module IDE.IfaceOld where

-- [ ] mention https://ghc.haskell.org/trac/ghc/wiki/ModuleReexports
-- [ ] mention backpack
import System.IO
import GHC.PackageDb -- https://github.com/ghc/ghc/blob/master/libraries/ghc-boot/GHC/PackageDb.hs#L140
import Binary -- https://github.com/ghc/ghc/blob/master/compiler/utils/Binary.hs
import Data.Maybe
import HscTypes -- https://github.com/ghc/ghc/blob/master/compiler/main/HscTypes.hs#L725
import Avail -- https://github.com/ghc/ghc/blob/master/compiler/basicTypes/Avail.hs#L37
import Name -- https://github.com/ghc/ghc/blob/master/compiler/basicTypes/Name.hs#L37
import Module -- https://github.com/ghc/ghc/blob/master/compiler/basicTypes/Module.hs#L248
import LoadIface
import GHC
import GHC.Paths ( libdir )
import DynFlags
import Control.Monad.IO.Class(liftIO)
import Outputable (dot)
import Data.List.Split
import Outputable -- https://github.com/ghc/ghc/blob/8c5fe53b411d83279fea44f89538a7265b1275ff/compiler/utils/Outputable.hs
import DynFlags (defaultDynFlags)
import BinIface
import TcRnMonad
import IfaceSyn  -- (ifType, ifName)
import Control.Monad (forM_)
import Data.Char (toLower, toUpper)
import System.Directory (createDirectoryIfMissing)
import Data.List (intersperse)
import Data.String.Utils
-- import Data.Tuple.Extra (both)
both f (a,b) = (f a, f b)

text_Just = Just

type Renderer = SDoc -> String

pretify :: Renderer -> AvailInfo -> [String]
pretify toS exportedName = case exportedName of
 Avail n -> [asString n]
 AvailTC _ xs -> map asString xs
 where asString = toS . ppr

whenValid "" = error "prefix can't be empty"
whenValid x =
  if all (`elem` (['a'..'z']++['A'..'Z'])) x
    then id
    else error "invalid chars"

test = printReexports ("lens", "/Users/RemiVion/.stack/snapshots/x86_64-osx/nightly-2015-11-29/7.10.2/lib/x86_64-osx-ghc-7.10.2/microlens-platform-0.1.5.0-GZu1yvU44tYD8BDHxEQWch/Lens/Micro/Platform.hi") []
printReexports :: (String, String) -> [String] -> IO [String]
printReexports (prefix, hiFilepath) previouslyExportedSymbols = whenValid prefix
  defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
    runGhc (Just libdir) $ do
      dflags <- getSessionDynFlags
      pkgs   <- setSessionDynFlags dflags
      sess   <- getSession

      -- load .hi interface file
      iface <- liftIO $ initTcRnIf 's' sess () () $
         readBinIface IgnoreHiWay QuietBinIFaceReading hiFilepath

      let
        toS :: SDoc -> String
        toS = showSDoc dflags
        exports = mi_exports iface
        exportedSymbols = concatMap (pretify toS) exports
        decls =  map snd $ mi_decls iface
        moduleName = toS.ppr $ mi_module iface
        jetpackFolder = "jetpack/src/"
        _folders = jetpackFolder ++ (replace "." "/" moduleName)
          -- let parts = splitOn "." moduleName
          -- in (concat$ intersperse "/" (init parts), last parts)
        (p:ps) = prefix
        sep = "_"
        _idPrefix = (toLower p : ps)
        _typePrefix = (toUpper p : ps)
        _fileName = concat ["As", _typePrefix]
        _filePath = (_folders ++ "/" ++ _fileName ++ ".hs")

      -- should we import the module qualified to avoid clashes with prelude ?
      -- if so, type signatures might not work anymore.
      -- are they working as of now anyway ?
      -- liftIO $ writeFile _filePath ""
      -- liftIO $ print (_folders,_file)
      liftIO $ createDirectoryIfMissing True _folders

      liftIO $ withFile _filePath WriteMode $ \fileHandle -> do
        let put = hPutStrLn fileHandle . concat
        put $
          [ "{-# LANGUAGE NoMonomorphismRestriction #-}"
          , "\n\nmodule ",moduleName,".", _fileName ," where"
          , "\n-- generated by rvion/jetpack-gen ", "\n"
          , "\n", "import ", moduleName, " as I"
          , "\n"
          ]

        -- putStrLn (toS.ppr$head $mi_exports iface)
        -- http://haddock.stackage.org/nightly-2015-12-09/ghc-7.10.2/GHC.html#t:ModIface
        -- http://haddock.stackage.org/nightly-2015-12-09/ghc-7.10.2/IfaceSyn.html#t:IfaceDecl
        newDecl <- (flip mapM) decls $ \decl -> do
          let
            _name = toS $ ppr (ifName decl)
            _reexported_name = concat [_idPrefix, sep, _name]
            _type = typeSdoc decl

          case () of
            () | not (_name `elem` exportedSymbols) -> do
                    putStrLn $ concat ["  warn: ", _name, " is not exported by current module"]
                    return Nothing
               | (_reexported_name `elem` previouslyExportedSymbols) -> do
                    putStrLn $ concat ["  warn: (",_reexported_name, ") previously exported"]
                    return Nothing
               | (head _name) `elem` operators -> do
                    putStrLn $ concat ["  warn: (", _name, ") is not reexported because it is an operator"]
                    return Nothing
               | otherwise -> do
                  put $ case decl of
                    IfaceId{} ->
                      -- ["\n", _idPrefix, sep, _name, " :: ", replace "\n" "\n  " (toS _type)
                      [_reexported_name, " = I.", _name]
                    IfaceData{} ->
                      ["type ", _typePrefix,sep, _name, " = I.", _name]
                    IfaceSynonym{} -> ["-- (",_name,") :: IfaceSynonym -> NOT YET SUPPORTED"]
                    IfaceFamily{} -> ["-- (",_name,") :: IfaceFamily -> NOT YET SUPPORTED"]
                    IfaceClass{} -> ["-- (",_name,") :: IfaceClass -> NOT YET SUPPORTED"]
                    IfaceAxiom{} -> ["-- (",_name,") :: IfaceAxiom -> NOT YET SUPPORTED"]
                    IfacePatSyn{} -> ["-- (",_name,") :: IfacePatSyn -> NOT YET SUPPORTED"]
                  return (Just _reexported_name)
        -- print (previouslyExportedSymbols)
        return (previouslyExportedSymbols ++ (catMaybes newDecl))


typeSdoc :: IfaceDecl -> SDoc
typeSdoc x =
  if isIfaceId x
    then ppr (ifType x)
    else ppr "-"

isIfaceId (IfaceId{}) = True
isIfaceId _ = False

showIfaceConstr x = case x of
  IfaceId{} -> "IfaceId"
  IfaceData{} -> "IfaceData"
  IfaceSynonym{} -> "IfaceSynonym"
  IfaceFamily{} -> "IfaceFamily"
  IfaceClass{} -> "IfaceClass"
  IfaceAxiom{} -> "IfaceAxiom"
  IfacePatSyn{} -> "IfacePatSyn"
          -- _ -> "ERROR"


-- http://downloads.haskell.org/~ghc/latest/docs/html/libraries/ghc-7.10.2/LoadIface.html

-- fp :: FilePath
-- fp = "/Users/rvion/.stack/snapshots/x86_64-osx/nightly-2015-11-29/7.10.2/lib/x86_64-osx-ghc-7.10.2/text-1.2.1.3-1l1AN4I48k37RaQ6fm6CEh/Data/Text.hi"
-- fp = "/Users/rvion/.stack/snapshots/x86_64-osx/nightly-2015-11-29/7.10.2/lib/x86_64-osx-ghc-7.10.2/tagged-0.8.2-4zanMqQLQHpBO0ZYm7KGkc/Data/Tagged.hi"
-- fp = "/Users/rvion/.stack/snapshots/x86_64-osx/nightly-2015-11-29/7.10.2/lib/x86_64-osx-ghc-7.10.2/zlib-0.5.4.2-7EfFFsXSCF6JCVS3xlYBS8/Codec/Compression/Zlib/Raw.hi"
-- fp = "/Users/rvion/.stack/snapshots/x86_64-osx/nightly-2015-11-10/7.10.2/lib/x86_64-osx-ghc-7.10.2/text-1.2.1.3-1l1AN4I48k37RaQ6fm6CEh/Data/Text.hi"

-- c :: IO ModIface
-- c = do
--   binHandle <- readBinMem fp
--   !a <- get binHandle
--   return a

-- d :: IO [AvailInfo] -- IO [IfaceExport]
-- d = mi_module <$> c
-- showSDoc

-- e = d >>= print.(showSDocUnsafe.ppr).head


-- instance Show ModuleName
--   where show a = show (toStringRep a)

-- instance Show Module where
--   show = show.toStringRep.moduleName
-- deriving instance Show Module
-- deriving instance Show Name
-- deriving instance Show IfaceExport

-- myShow x = show (mi_module x)
-- myshowforName a = case a of
--   Avail  name -> show name
--   AvailTC name fieldLabels -> show name
for :: [a] -> (a->b) -> [b]
for = flip map

operators :: String
operators = ['!','#','$','%','&','*','+','.','/','<','=','>','?','@','\\','^','|','-','~',':']