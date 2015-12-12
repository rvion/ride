module IDE.JetpackGen.Modules where

import Avail -- https://github.com/ghc/ghc/blob/master/compiler/basicTypes/Avail.hs#L37
import BinIface
import Control.Monad (forM)
import Data.Char (toLower, toUpper, isLower, isUpper)
import Name
import Data.List
import Data.Maybe
import Data.String.Utils
import DynFlags
import GHC
import GHC.Paths ( libdir )
import IDE.Types
import IfaceSyn  -- (ifType, ifName)
import Outputable -- https://github.com/ghc/ghc/blob/8c5fe53b411d83279fea44f89538a7265b1275ff/compiler/utils/Outputable.hs
import qualified Data.Map as Map
import System.Directory (createDirectoryIfMissing)
import System.IO
import TcRnMonad
import Data.Map (Map)


whenValid "" = error "prefix can't be empty"
whenValid x =
  if all (`elem` ('_':['a'..'z']++['A'..'Z'])) x
    then id
    else error "invalid chars"

-- printReexports
printReexports :: (String, String) -> [RTerm] -> [String] -> IO [String]
printReexports (mod, prefix) reexports previouslyExportedSymbols = do
  let
    moduleName = mod -- TODO
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

  createDirectoryIfMissing True _folders
  withFile _filePath WriteMode $ \fileHandle -> do
    let put = hPutStrLn fileHandle . concat

    put $
      [ "module ",moduleName,".", _fileName ," where"
      , "\n-- generated by rvion/jetpack-gen ", "\n"
      , "\n", "import ", moduleName, " as I"
      , "\n"
      ]

    newDecl <- forM reexports $ \rTerm -> do
      let
        _name = rName rTerm
        _reexported_name = concat [_idPrefix, sep, _name]
        _reexported_type = concat [_typePrefix, _name]
        -- _type = typeSdoc decl

      if (_reexported_name `elem` previouslyExportedSymbols)
        then do
          putStrLn $ concat ["  warn: (",_reexported_name, ") previously exported"]
          return Nothing
        else case rTerm of
          RId _
            | (head _name) `elem` operators -> do
                put ["(",_name,")", " = (I.", _name,")"]
                print _name
                return (Just _name)
            | isLower (head _name) || (head _name) == '_' -> do
                put [_reexported_name, " = I.", _name]
                return (Just _reexported_name)
            | otherwise -> error "ahaha"
              -- RSynonym _ _ -> error "not yet SUPPORTED"
          RData _ nbTyVars -> do
              let tyVars = intersperse ' ' $ take nbTyVars ['a'..'z']
              put ["type ", _reexported_type," ",tyVars, " = I.", _name, " ",tyVars]
              return (Just _reexported_type) -- tyvars needed because type synonym must be instanciated
              -- _  -> do
              --   put ["type ", _reexported_type, " = I.", _name, " -- not declared in module, :/ ? "]
              --   return (Just _reexported_type)
          -- _ -> do
                -- return Nothing

                -- put $ case decl of
                --   IfaceId{} ->
                -- ["\n", _idPrefix, sep, _name, " :: ", replace "\n" "\n  " (toS _type)
                -- put [_reexported_name, " = I.", _name]
                -- IfaceData{} ->
                -- put ["type ", _typePrefix,sep, _name, " = I.", _name]
                -- return (Just _reexported_name)
                -- IfaceSynonym{} -> ["-- (",_name,") :: IfaceSynonym -> NOT YET SUPPORTED"]
                -- IfaceFamily{} -> ["-- (",_name,") :: IfaceFamily -> NOT YET SUPPORTED"]
                -- IfaceClass{} -> ["-- (",_name,") :: IfaceClass -> NOT YET SUPPORTED"]
                -- IfaceAxiom{} -> ["-- (",_name,") :: IfaceAxiom -> NOT YET SUPPORTED"]
                -- IfacePatSyn{} -> ["-- (",_name,") :: IfacePatSyn -> NOT YET SUPPORTED"]
    -- print (previouslyExportedSymbols)
    return (previouslyExportedSymbols ++ (catMaybes newDecl))

-- handleDeprecated :: RTerm -> IO (Maybe String)
-- handleDeprecated rTerm = do
--   putStrLn $ concat ["  warn: (",_reexported_name,") not exported because it is deprecated"]
--   return Nothing

operators :: String
operators = ['!','#','$','%','&','*','+','.','/','<','=','>','?','@','\\','^','|','-','~',':']


-- liftIO $ mapM_ (putStrLn.(" -> "++).toS) declsF

