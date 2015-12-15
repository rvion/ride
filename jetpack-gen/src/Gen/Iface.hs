{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# LANGUAGE Rank2Types           #-}

module Gen.Iface where

import           Avail
import           BinIface
import           Control.Monad     (forM)
import           Data.List         (intersperse)
import           Data.Map          (Map)
import Debug.Trace
import qualified Data.Map          as Map
import           Data.Maybe
import           Data.String.Utils
import           DynFlags
import           Gen.Types
import           GHC
import           GHC.Paths         (libdir)
import           IfaceSyn
import           Name
import           Outputable
import           TcRnMonad

type Renderer = SDoc -> String
type IfaceDeclMap = Map String IfaceDecl

data DeclLoc = Remote FilePath | Local | NotFound deriving (Eq, Show)
getIface :: String -> Ghc ModIface
getIface hiFilepath = do
  sess <- getSession
  liftIO $ initTcRnIf 's' sess () () $
    readBinIface IgnoreHiWay QuietBinIFaceReading hiFilepath

-- getIfaceDecl :: Renderer -> Name -> ModIface -> IfaceDecl
-- getIfaceDecl toS name modIface =
--   let modIfaceMap = mkIfaceDeclMap toS modIface
--   in  modIfaceMap Map.! (toS.ppr$name)

mkIfaceDeclMap :: Renderer -> ModIface -> IfaceDeclMap
mkIfaceDeclMap toS iface =
  Map.fromList (map toTupple ifaceDecls)
  where
    toTupple ifaceDecl = (nameAsString ifaceDecl, ifaceDecl)
    ifaceDecls = map snd $ mi_decls iface
    nameAsString = toS . ppr . ifName

findReexports :: (String, Modules) -> [String] -> IO [RTerm]
findReexports (mod, modules) previouslyExportedSymbols =
  defaultErrorHandler defaultFatalMessager defaultFlushOut $
    runGhc (Just libdir) $ do
  dflags <- getSessionDynFlags
  pkgs   <- setSessionDynFlags dflags
  sess   <- getSession

  let
    -- define context-dependant helper functions
    toSDoc :: SDoc -> String
    toSDoc = showSDoc dflags
    toS :: Outputable a => a -> String
    toS = toSDoc . ppr

  -- load the initial .hi interface file
  iface <- getIface (modules Map.! mod)

  let
    -- we want to reexport all IfacesExport,
    ifaceExports = mi_exports iface
    exportedSymbols = concatMap extractAllNames ifaceExports
    decls = mi_decls iface
    declsMap = mkIfaceDeclMap toSDoc iface
    moduleName = toS $ mi_module iface
    _folders = jetpackLibFolder ++ replace "." "/" moduleName
  liftIO . putStrLn $ concat ["  exports are ",toS exportedSymbols]

  -- find all decls corresponding to names
  declsF <- forM exportedSymbols $ \name -> do
    -- liftIO$putStrLn("  trying to find "++toS name)
    let
      _name = toS name
      _success ifaceDecl loc = return (name, Just ifaceDecl, loc)
      _failDeprecated loc = do
        liftIO.putStrLn.concat$["  info: (",toS name,") is not reexported because it is deprecated."]
        return (name, Nothing, loc)
      _fail = do
        liftIO.putStrLn.concat$["  warn: impossible to find decl for (",toS name,")"]
        return (name, Nothing, NotFound)

    case Map.lookup _name declsMap of
      Just ifaceDecl ->
        if isJust (mi_warn_fn iface name)
          then _failDeprecated Local
          else _success ifaceDecl Local
      Nothing ->
        case nameModule_maybe name of
          Nothing -> _fail
          Just nameModule -> do -- eg: Data.Either
            let nameModuleStr = toS nameModule
                -- xx= Mod.moduleName nameModule
                -- nameModuleFS  = moduleNameFS $ Mod.moduleName nameModule
            -- liftIO $ error $ toS nameModuleFS
            -- liftIO $ putStrLn ("    - loading "++ nameModuleStr)
            case Map.lookup nameModuleStr modules of
              Nothing -> do
                liftIO $ putStr $ concat ["\n  module (",nameModuleStr,") is really nowhere... :/"]
                _fail
              Just _ifacePath -> do
                _otherIface <- getIface _ifacePath
                let _otherIfaceMap = mkIfaceDeclMap toSDoc _otherIface
                case Map.lookup _name _otherIfaceMap of
                  Just otherIfaceDecl ->
                    if isJust (mi_warn_fn _otherIface name)
                      then _failDeprecated (Remote _ifacePath)
                      else _success otherIfaceDecl (Remote _ifacePath)
                  Nothing -> _fail

  -- liftIO $ mapM_ (print) (map (\(a,b,c)->(toS a, maybe "" showIfaceInfo b)) $ declsF)
  let onOneLine = unwords . lines
  return $ catMaybes <$> for declsF $ \(n, decl, loc) ->
    case decl of
      Nothing -> Nothing
      Just (_t@(IfaceId{})) -> Just $
        RId
          (toS n)
          (onOneLine $ toS (ifType _t))
      Just (_t@(IfaceData{})) -> Just
        RData
          { rName = toS n
          , rType = onOneLine $ toS (ifType _t)
          , rNbTyVars = length (ifTyVars _t)
          }
      Just (_t@(IfaceSynonym{})) -> Just
        RData
          { rName = toS n
          , rType = onOneLine $ toS (ifType _t)
          , rNbTyVars = length (ifTyVars _t)
          }
      Just (_t@(IfaceClass{})) -> case loc of
        -- Remote x -> traceShow (Remote x, toS _t) Nothing
        _ -> Just $ RClass
          ( toS n)
          (map
            (\(IfaceClassOp n' _ t') -> RId (toS n') (onOneLine.toS$t'))
            (ifSigs _t)
          )
          -- FIXME the solution seems to be to reexport class memebres only when
          -- they are reexported alng the class...

          -- in other situations, we could just export something like that:
          --   module Options.Applicative.AsOpt
          --     ( module Options.Applicative.AsOpt
          --     , I.Applicative
          --     ) where

      Just (IfaceFamily{}) -> Nothing
      Just (IfaceAxiom{}) -> Nothing
      Just (IfacePatSyn{}) -> Nothing


-- isDeprecated n = mi_warn_fn iface
-- for = flip map

typeSdoc :: IfaceDecl -> SDoc
typeSdoc x =
  if isIfaceId x
    then ppr (ifType x)
    else ppr "-"

isIfaceId (IfaceId{}) = True
isIfaceId _ = False


-- toReexportableTerm :: IfaceDecl -> ReexportableTerm
-- toReexportableTerm x =

showIfaceKind :: IfaceDecl -> String
showIfaceKind x = case x of
  IfaceId{} -> "IfaceId"
  IfaceData{} -> "IfaceData"
  IfaceSynonym{ifTyVars} -> "IfaceSynonym: "
  IfaceFamily{} -> "IfaceFamily"
  IfaceClass{} -> "IfaceClass"
  IfaceAxiom{} -> "IfaceAxiom"
  IfacePatSyn{} -> "IfacePatSyn"
          -- _ -> "ERROR"

showIfaceInfo :: IfaceDecl -> String
showIfaceInfo x = case x of
  IfaceId{} -> "IfaceId"
  IfaceData{} -> "IfaceData"
  IfaceSynonym{} -> "IfaceSynonym"
  IfaceFamily{} -> "IfaceFamily"
  IfaceClass{} -> "IfaceClass"
  IfaceAxiom{} -> "IfaceAxiom"
  IfacePatSyn{} -> "IfacePatSyn"

for :: [a] -> (a->b) -> [b]
for = flip map

extractAllNames :: AvailInfo -> [Name]
extractAllNames ai = case ai of
 Avail n -> [n]
 AvailTC x xs -> [x] -- :xs -- TODO


-- test = printReexports ("lens", "/Users/RemiVion/.stack/snapshots/x86_64-osx/nightly-2015-11-29/7.10.2/lib/x86_64-osx-ghc-7.10.2/microlens-platform-0.1.5.0-GZu1yvU44tYD8BDHxEQWch/Lens/Micro/Platform.hi") []

-- http://downloads.haskell.org/~ghc/latest/docs/html/libraries/ghc-7.10.2/LoadIface.html
-- fp :: FilePath
-- fp = "/Users/rvion/.stack/snapshots/x86_64-osx/nightly-2015-11-29/7.10.2/lib/x86_64-osx-ghc-7.10.2/text-1.2.1.3-1l1AN4I48k37RaQ6fm6CEh/Data/Text.hi"
-- fp = "/Users/rvion/.stack/snapshots/x86_64-osx/nightly-2015-11-29/7.10.2/lib/x86_64-osx-ghc-7.10.2/tagged-0.8.2-4zanMqQLQHpBO0ZYm7KGkc/Data/Tagged.hi"
-- fp = "/Users/rvion/.stack/snapshots/x86_64-osx/nightly-2015-11-29/7.10.2/lib/x86_64-osx-ghc-7.10.2/zlib-0.5.4.2-7EfFFsXSCF6JCVS3xlYBS8/Codec/Compression/Zlib/Raw.hi"
-- fp = "/Users/rvion/.stack/snapshots/x86_64-osx/nightly-2015-11-10/7.10.2/lib/x86_64-osx-ghc-7.10.2/text-1.2.1.3-1l1AN4I48k37RaQ6fm6CEh/Data/Text.hi"

-- liftIO $ print $ toS $ (map getNameInfos

-- should we import the module qualified to avoid clashes with prelude ?
-- if so, type signatures might not work anymore.
-- are they working as of now anyway ?
-- liftIO $ writeFile _filePath ""
-- liftIO $ print (_folders,_file)
