{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# LANGUAGE Rank2Types           #-}

module Gen.Iface where

import           Avail
import           BinIface
import           Control.Monad     (forM)
import           Data.List
import           Data.Map          (Map)
import qualified Data.Map          as Map
import           Data.Maybe
import           Data.String.Utils
import           Debug.Trace
import           DynFlags
import           Gen.Types
import           GHC
import           GHC.Paths         (libdir)
import           IfaceSyn
import           Name
import           Outputable
import           TcRnMonad

type Renderer = SDoc -> String
type IfaceDeclMap = Map OccName IfaceDecl
data DeclLoc = Remote FilePath | Local | NotFound deriving (Eq, Show)
type ModuleName = String

-- data G = G1 Int
-- pattern JsG1 <- G1
-- mkG1 = G1

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
    nameAsString = ifName

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
  currentIface <- getIface (modules Map.! mod)

  let
    -- we want to reexport all IfacesExport,
    ifaceExports :: [AvailInfo]
    ifaceExports = mi_exports currentIface
    exportedSymbols = concatMap extractAllNames ifaceExports
    decls = mi_decls currentIface
    declsMap = mkIfaceDeclMap toSDoc currentIface
    moduleName = toS $ mi_module currentIface
    _folders = jetpackLibFolder ++ replace "." "/" moduleName
  let
    allAvailNames = map availName ifaceExports
    allNecessaryModules = map toS $ nub $ mapMaybe nameModule_maybe allAvailNames
    -- allAvailNamesAndFP = for ifaceExports (\n -> (n, toS $ nameModule.availName$ n)) -- nameModule_maybe -- dangerous
  all_modules <- foldlM (\m x ->
    case Map.lookup x modules of
      Just mfp -> do
        ifa <- getIface mfp
        return $ Map.insert x (mkIfaceDeclMap toSDoc ifa) m
      Nothing -> return m) Map.empty allNecessaryModules

  -- liftIO . putStrLn $ concat ["\n  Exports are ", toS ifaceExports]

  -- liftIO . putStrLn $ concat ["  exports are ", toS (nub $ catMaybes $ map nameModule_maybe (concatMap availNames ifaceExports))]
  -- liftIO . putStrLn $ concat ["\n  Names are ", toS (map nameOccName allAvailNames)]
  -- liftIO . putStrLn $ concat ["\n  Modules are ", show allNecessaryModules]
  -- liftIO . putStrLn $ concat ["\n  names and fp are ", show $ map (\(x,y) -> (toS x,y)) allAvailNamesAndFP]
  -- liftIO $ do
  --   putStrLn $ concat ["\n  names and fp are \n"]
  --   forM (Map.assocs all_modules) $ \(x,y)-> do
  --     putStrLn x
  --     putStrLn $ toS (Map.keys y)

  -- liftIO . putStrLn $ concat ["  exports are ", toS ifaceExports]

  -- find all decls corresponding to names
  declsF <- forM ifaceExports $ \availInfo -> do
    -- liftIO$putStrLn("  trying to find "++toS availInfo)
    let
      name = availName availInfo
      moduleName = toS . nameModule . availName $ availInfo
      -- _name = toS name

      _success x loc = return (name, Just x, loc)
      _failDeprecated loc = do
        liftIO.putStrLn.concat$["  info: (",toS name,") is not reexported because it is deprecated."]
        return (name, Nothing, loc)
      _fail = do
        liftIO.putStrLn.concat$["  warn: impossible to find decl for (",toS name,")"]
        return (name, Nothing, NotFound)

    -- liftIO$putStrLn("  looking at  "++ show (toS name,moduleName))
    case Map.lookup (nameOccName name) (all_modules Map.! moduleName) of
      Just ifaceDecl ->
        if isJust (mi_warn_fn currentIface name)
          then _failDeprecated Local
          else
            _success (ifaceDecl, availNames availInfo) Local
      Nothing ->
        liftIO _fail
  -- liftIO . putStrLn $ concat ["  exports are ", toS (map (\(a,xx,_) -> (a, case xx of {Just (aaa,bbb) -> showIfaceInfo aaa; _ -> ""})) declsF)]

      -- Nothing ->
      --   case nameModule_maybe name of
      --     Nothing -> _fail
      --     Just nameModule -> do -- eg: Data.Either
      --       let nameModuleStr = toS nameModule
      --           -- xx= Mod.moduleName nameModule
      --           -- nameModuleFS  = moduleNameFS $ Mod.moduleName nameModule
      --       -- liftIO $ error $ toS nameModuleFS
      --       -- liftIO $ putStrLn ("    - loading "++ nameModuleStr)
      --       case Map.lookup nameModuleStr modules of
      --         Nothing -> do
      --           liftIO $ putStr $ concat ["\n  module (",nameModuleStr,") is really nowhere... :/"]
      --           _fail
      --         Just _ifacePath -> do
      --           _otherIface <- getIface _ifacePath
      --           let _otherIfaceMap = mkIfaceDeclMap toSDoc _otherIface
      --           case Map.lookup _name _otherIfaceMap of
      --             Just otherIfaceDecl ->
      --               if isJust (mi_warn_fn _otherIface name)
      --                 then _failDeprecated (Remote _ifacePath)
      --                 else _success otherIfaceDecl (Remote _ifacePath)
      --             Nothing -> _fail
  -- liftIO $ print $ toS exportedNames
  -- liftIO $ mapM_ (print) (map (\(a,b,c)->(toS a, maybe "" showIfaceInfo b)) $ declsF)

  let onOneLine = unwords . lines
  return $ catMaybes <$> for declsF $ \(n, decl, loc) ->
    case decl of
      Nothing -> Nothing
      Just (_t@(IfaceId{}), exportedNames) -> Just $
        RId
          (toS n)
          (onOneLine $ toS (ifType _t))
      Just (_t@(IfaceData{}), exportedNames) -> Just
        RData
          { rName = toS n
          , rType = onOneLine $ toS (ifType _t)
          , rNbTyVars = length (ifTyVars _t)
          , rDataTyCon = case ifCons _t of
              IfDataTyCon cons -> -- mapMaybe (undefined)
                catMaybes $ for cons $ \ con -> -- error (toS (ifConOcc con) ++ (toSDoc $ pprIfCon con))$
                  if ifConOcc con `elem` map nameOccName exportedNames
                  then Just
                    RDataCon
                      { rName = toS (ifConOcc con)
                      , rTyVars = map toS (ifConArgTys con)
                      , rNbTyVars = length $ ifConArgTys con
                    }
                  else Nothing
              _ -> []
          }
      Just (_t@(IfaceSynonym{}), exportedNames) -> Just
        RData
          { rName = toS n
          , rType = onOneLine $ toS (ifType _t)
          , rNbTyVars = length (ifTyVars _t)
          , rDataTyCon = []
          }
      Just (_t@(IfaceClass{ifName}), exportedNames) -> case loc of
        Remote x -> error (show (Remote x, toS _t))
        -- if toS n == "FromJSON" then error (show (toS n, toS decl, loc)) else
        _ -> Just $ RClass
          ( toS ifName ) -- ARGH, don't do (toS n) here...
          (ifName `elem` (map nameOccName exportedNames))
          (mapMaybe
            (\(IfaceClassOp n' _ t') -> if n' `elem` map nameOccName exportedNames
                then Just $  RId (toS n') (onOneLine.toS$t')
                else Nothing)
            (ifSigs _t)
          )

      Just (IfaceFamily{},_) -> Nothing
      Just (IfaceAxiom{},_) -> Nothing
      Just (IfacePatSyn{},_) -> Nothing

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

-- showAllNames :: AvailInfo -> [Name]
-- extractAllNames ai = case ai of
--  Avail n -> [n]
--  AvailTC x xs -> (x:xs) -- TODO

extractAllNames :: AvailInfo -> [Name]
extractAllNames ai = case ai of
 Avail n -> [n]
 AvailTC x xs -> x:xs -- TODO


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

pprIfCon x = vcat
  [ ppr (ifConOcc x)
  , ppr (ifConWrapper x)
  , ppr (ifConInfix x)
  , ppr (ifConExTvs x)
  , ppr (ifConEqSpec x)
  , ppr (ifConCtxt x)
  , ppr (ifConArgTys x)
  , ppr (ifConFields x)
  -- , ppr (ifConStricts x)
  ]
