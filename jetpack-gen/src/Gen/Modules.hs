{-# LANGUAGE RecordWildCards #-}
module Gen.Modules where

import           Control.Monad     (forM)
import           Data.Char         (isLower, toLower, toUpper)
import           Data.List
import           Debug.Trace
import           Data.Maybe
import           Control.Monad
import           Data.String.Utils
import           Gen.Types
import           Gen.Log
import           System.Directory  (createDirectoryIfMissing)
import           System.IO


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
    _folders = jetpackLibFolder ++ replace "." "/" moduleName
      -- let parts = splitOn "." moduleName
      -- in (concat$ intersperse "/" (init parts), last parts)
    (p:ps) = prefix
    sep = "_"
    _idPrefix = (toLower p : ps) ++ sep
    _typePrefix = toUpper p : ps
    _fileName = "As" ++ _typePrefix
    _filePath = _folders ++ "/" ++ _fileName ++ ".hs"

  createDirectoryIfMissing True _folders
  withFile _filePath WriteMode $ \fileHandle -> do
    let put = hPutStrLn fileHandle . concat

    let
      -- core reexport codegen
      reexportFn x = do
        let
          _name = rName x
          _reexported_name =
            if head _name `elem` operators
              then _name
              else _idPrefix ++ _name
          _reexported_type = _typePrefix ++ _name
          -- _type = typeSdoc decl
        case x of
          RId _ rType
            | _reexported_name `elem` previouslyExportedSymbols -> do
                -- putStrLn $ concat ["  warn: (",_reexported_name, ") previously exported"]
                return [Nothing]
            | head _name `elem` operators -> do
                put ["\n-- (",_name,") :: ",rType]
                put ["(",_name,")", " = (I.", _name,")"]
                return [Just _reexported_name]
            | isLower (head _name) || head _name == '_' -> do
                put ["\n-- ",_reexported_name," :: ",rType]
                put [_reexported_name, " = I.", _name]
                return [Just _reexported_name]
            | otherwise -> error ("ahaha " ++ _name)
          RData _ rType nbTyVars rDataTyCons
            | _reexported_type `elem` previouslyExportedSymbols -> do
                asWarning $ putStrLn $ concat ["  warn: (",_reexported_name, ") previously exported"]
                return [Nothing]
            | otherwise -> do
                -- print
                let tyVars = intersperse ' ' $ take nbTyVars ['a'..'z']
                -- put ["-- ",_reexported_type," :: ",rType]
                put (["\ntype ", _reexported_type," ",tyVars, " = I.", _name] ++ (if nbTyVars > 0 then [" ",tyVars] else []))
                exportedCons <- concat <$> forM rDataTyCons reexportFn
                return (Just _reexported_type:exportedCons) -- tyvars needed because type synonym must be instanciated
          RDataCon{..}
            | head rName `elem` operators -> do
                asWarning $ putStrLn $ concat ["  warn: (",rName, ") as a type constructor is not yet supported"]
                return [Nothing]
            | otherwise -> do
                print (rName, zip rConFields rTyVars)
                let tyVars = intersperse ' ' $ take rNbTyVars ['a'..'z']
                let constrType = intercalate " -> " (rTyVars ++ [rName])
                let conName = concat [_idPrefix,"mk'", rName]
                let patName = _typePrefix ++ rName
                getsets <- forM rConFields $ \confield -> do
                  let
                    _getter = concat ["get_", _idPrefix, confield]
                    _setter = concat ["set_", _idPrefix, confield]
                  put [_getter, " o = I.", confield, " o"]
                  put [_setter, " x o = o { I.", confield, " = x}"]
                  return [Just _getter, Just _setter]
                conss <- if rConIsReexported
                  then do
                    put ["\n-- constructor :: ", constrType]
                    put [conName, " =  I.", rName]
                    put ["pattern ", patName, " ", tyVars, " <-  I.", rName, " ", tyVars]
                    return [Just conName, Just ("pattern " ++ patName)]
                  else return []
                return (conss ++ (concat getsets))
          RClass n nexpo fns -> do
            print ("oksssssssss", RClass n nexpo fns)
            return [Nothing]
            -- concat <$> forM fns reexportFn

    -- print "------------"
    -- print $map (length.rName) reexports
    -- print $map (rName) reexports
    -- print "------------"

    let (allClassesRTerm, otherRTerms) = partition isRClass reexports
    let (classesRTerm, alreadyExportedClassesRTerm) = partition (\x -> not $ (rName x) `elem` previouslyExportedSymbols) allClassesRTerm
    forM alreadyExportedClassesRTerm $ \x -> asWarning $ putStrLn $ concat ["  warn: name of class member(",rName x, ") previously exported"]
    -- print "rv"
    -- print ("classes: ", map (rName) classesRTerm)
    -- print ("func & types: ", map (rName) otherRTerms)
    put
      [ "module ",moduleName,".", _fileName
      , "\n  ( "
      ,  if (not . null $ classesRTerm)
          then "-- unqualified class re-export\n  " ++ (intercalate ", " (map toClassExp classesRTerm)) ++ "\n  , "
          else ""
      , "module ",moduleName,".", _fileName
      , "\n  ) where"
      , "\n-- generated by https://github.com/rvion/ride/tree/master/jetpack-gen"
      , "\n\nimport qualified ", moduleName, " as I"
      , "\n"
      ]

    let newClassDecls = concatMap getAllClassesNames classesRTerm
    newDecl <- forM otherRTerms $ \rTerm -> do
      reexportFn rTerm

    -- print (previouslyExportedSymbols)
    return (previouslyExportedSymbols ++ newClassDecls ++ catMaybes(concat newDecl))

-- handleDeprecated :: RTerm -> IO (Maybe String)
-- handleDeprecated rTerm = do
--   putStrLn $ concat ["  warn: (",_reexported_name,") not exported because it is deprecated"]
--   return Nothing

operators :: String
operators = ['!','#','$','%','&','*','+','.','/','<','=','>','?','@','\\','^','|','-','~',':']

getAllClassesNames :: RTerm -> [String]
getAllClassesNames x = if (rNameExported x) then ((rName x):(map rName (rAssos x))) else (map rName (rAssos x))

rNameClever x =
  let _name = (rName x)
  in  if head _name `elem` operators
        then ("(I." ++ _name ++ ")")
        else ("I."++_name)

toClassExp :: RTerm -> String
toClassExp (RClass s nexpo t) = if nexpo
  then concat $["I.", s,"("] ++ (intersperse ", " (map rNameClever t)) ++ [")"] -- Typeclass itself is not exported http://stackoverflow.com/questions/17849870/closed-type-classes
  else concat $ (intersperse ", " (map rNameClever t))

-- liftIO $ mapM_ (putStrLn.(" -> "++).toS) declsF

-- data G = G1 Int
-- pattern JsG1 <- G1
-- mkG1 = G1
