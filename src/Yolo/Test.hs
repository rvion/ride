{-#LANGUAGE StandaloneDeriving#-}
{-#LANGUAGE TypeSynonymInstances#-}
{-#LANGUAGE GeneralizedNewtypeDeriving#-}
{-#LANGUAGE BangPatterns#-}

module Yolo.Test where

import GHC.PackageDb -- https://github.com/ghc/ghc/blob/master/libraries/ghc-boot/GHC/PackageDb.hs#L140
import Binary -- https://github.com/ghc/ghc/blob/master/compiler/utils/Binary.hs
import HscTypes -- https://github.com/ghc/ghc/blob/master/compiler/main/HscTypes.hs#L725
import Avail -- https://github.com/ghc/ghc/blob/master/compiler/basicTypes/Avail.hs#L37
import Name -- https://github.com/ghc/ghc/blob/master/compiler/basicTypes/Name.hs#L37
import Module -- https://github.com/ghc/ghc/blob/master/compiler/basicTypes/Module.hs#L248
import LoadIface
import GHC
import GHC.Paths ( libdir )
import DynFlags
import Control.Monad.IO.Class(liftIO)

import Outputable -- https://github.com/ghc/ghc/blob/8c5fe53b411d83279fea44f89538a7265b1275ff/compiler/utils/Outputable.hs
import DynFlags (defaultDynFlags)
import BinIface
import TcRnMonad

demo :: IO ()
demo =
  defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
    runGhc (Just libdir) $ do
      dflags <- getSessionDynFlags
      pkgs <- setSessionDynFlags dflags
      sess <- getSession
      iface <- liftIO $ initTcRnIf 's' sess () () $
         readBinIface IgnoreHiWay QuietBinIFaceReading fp
      -- liftIO $ showIface sess fp
      -- ss <- liftIO d
      liftIO $ print (showSDoc dflags (ppr (mi_exports iface)))

      return ()

      -- setSessionDynFlags dflags
      -- target <- guessTarget "test_main.hs" Nothing
      -- setTargets [target]
      -- load LoadAllTargets

-- http://downloads.haskell.org/~ghc/latest/docs/html/libraries/ghc-7.10.2/LoadIface.html

fp :: FilePath
fp = "/Users/rvion/.stack/snapshots/x86_64-osx/nightly-2015-11-29/7.10.2/lib/x86_64-osx-ghc-7.10.2/zlib-0.5.4.2-7EfFFsXSCF6JCVS3xlYBS8/Codec/Compression/Zlib/Raw.hi"
-- fp = "/Users/rvion/.stack/snapshots/x86_64-osx/nightly-2015-11-10/7.10.2/lib/x86_64-osx-ghc-7.10.2/text-1.2.1.3-1l1AN4I48k37RaQ6fm6CEh/Data/Text.hi"

c :: IO ModIface
c = do
  binHandle <- readBinMem fp
  !a <- get binHandle
  return a

-- d :: IO [AvailInfo] -- IO [IfaceExport]
d = mi_module <$> c
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
