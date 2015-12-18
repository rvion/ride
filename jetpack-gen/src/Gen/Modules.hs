{-# LANGUAGE RecordWildCards #-}
module Gen.Modules where
import           Control.Monad
import           Data.Char         (isLower, toLower, toUpper)
import           Data.List
import           Data.Maybe
import           Data.String.Utils
import           Debug.Trace
import           Gen.Log
import           Gen.Types
import           System.Directory  (createDirectoryIfMissing)
import           System.IO

-- reexportedNameFor :: Reexport -> RTerm -> String
-- reexportedNameFor r t = case r of
--   Qualified{..}
--     | isOperator t -> rName t
--     | otherwise ->
--       let
--         (p:ps) = as
--         sep = "_"
--         pref = (toLower p : ps) ++ sep
--         tpref = toUpper p : ps
--       in case t of
--         RId{..} -> concat [pref, sep, rName]
--         RData{..} -> concat [tpref, rName]
--         RDataCon{..} -> rName
--         RClass{..} -> rName
--   Unqualified{..} -> case t of
--     RId{..} -> "RId"
--     RData{..} -> "RData"
--     RDataCon{..} -> "RDataCon"
--     RClass{..} -> "RClass"


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
            | _reexported_name `elem` previouslyExportedSymbols ->
                return [Nothing]
            -- | head _name `elem` operators -> do
            --     put ["\n-- (",_name,") :: ",rType]
            --     put ["(",_name,")", " = (I.", _name,")"]
            --     return [Just _reexported_name]
            | isLower (head _name) || head _name == '_' -> do
                put ["\n-- ",_reexported_name," :: ",rType]
                put [_reexported_name, " = I.", _name]
                return [Just _reexported_name]
            | otherwise -> error ("ahaha " ++ _name)
          RData _ rType nbTyVars rDataTyCons
            | _reexported_type `elem` previouslyExportedSymbols -> do
                asWarning $ putStrLn $ concat
                  ["  warn: (",_reexported_name, ") previously exported"]
                return [Nothing]
            | otherwise -> do
                -- print
                let tyVars = intersperse ' ' $ take nbTyVars ['a'..'z']
                -- put ["-- ",_reexported_type," :: ",rType]
                put (["\ntype ", _reexported_type," ",tyVars, " = I.", _name] ++
                  (if nbTyVars > 0 then [" ",tyVars] else []))
                exportedCons <- concat <$> forM rDataTyCons reexportFn
                -- tyvars needed because type synonym must be instanciated
                return (Just _reexported_type:exportedCons)
          RDataCon{..} -- -> do
            | head rName `elem` operators -> do
                asWarning $ putStrLn $ concat
                  ["  warn: (",rName, ") as a type constructor is not yet supported"]
                return [Nothing]
            | otherwise -> do
                -- print (rName, zip rConFields rTyVars)
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
                return (conss ++ concat getsets)
          RClass n nexpo fns -> do
            print ("oksssssssss", RClass n nexpo fns)
            return [Nothing]
            -- concat <$> forM fns reexportFn

    -- print "------------"
    -- print $map (length.rName) reexports
    -- print $map (rName) reexports
    -- print "------------"

    let
      (allClassesRTerm, otherRTermsWithSymbols) = partition isRClass reexports
      (allSymbols, otherRTerms) =
        partition (\x-> head (rName x) `elem` operators) otherRTermsWithSymbols
      (classesRTerm, alreadyExportedClassesRTerm) =
        partition (\x -> notElem (rName x) previouslyExportedSymbols)
          allClassesRTerm
    let allSymbolsNotPreviouslyExported = filter (\sym -> not $ (rName sym) `elem` previouslyExportedSymbols) allSymbols
    forM_ alreadyExportedClassesRTerm $ \x -> asWarning $ putStrLn $ concat
      ["  warn: name of class member(",rName x, ") previously exported"]
    -- print "rv"
    -- print ("classes: ", map (rName) classesRTerm)
    -- print ("func & types: ", map (rName) otherRTerms)
    put
      [ "module ",moduleName,".", _fileName
      , "\n  ( "
      ,  if not . null $ classesRTerm
          then "-- unqualified class re-export\n  " ++
            intercalate ", " (map toClassExp classesRTerm) ++ "\n  , "
          else ""
      ,  if not . null $ allSymbolsNotPreviouslyExported
          then "-- unqualified operators re-export\n  " ++
            intercalate ", " (catMaybes $ map toOpExt allSymbolsNotPreviouslyExported) ++ "\n  , "
          else ""
      , "module ",moduleName,".", _fileName
      , "\n  ) where"
      , "\n-- generated by https://github.com/rvion/ride/tree/master/jetpack-gen"
      , "\n\nimport qualified ", moduleName, " as I"
      , "\n"
      ]

    let newOperatorDecl = map rName allSymbolsNotPreviouslyExported
    let newClassDecls = concatMap getAllClassesNames classesRTerm
    newDecl <- forM otherRTerms $ \rTerm ->
      reexportFn rTerm

    -- print (previouslyExportedSymbols)
    return $ concat
      [ previouslyExportedSymbols
      , newClassDecls
      , newOperatorDecl
      , catMaybes(concat newDecl)
      ]

-- handleDeprecated :: RTerm -> IO (Maybe String)
-- handleDeprecated rTerm = do
--   putStrLn $ concat
--     ["  warn: (",_reexported_name,") not exported because it is deprecated"]
--   return Nothing

operators :: [Char]
operators = ['!','#','$','%','&','*','+','.','/','<','=','>','?','@','\\','^','|','-','~',':']

isOperator :: RTerm -> Bool
isOperator t = head (rName t) `elem` operators

getAllClassesNames :: RTerm -> [String]
getAllClassesNames x =
  if rNameExported x
    then (rName x):(map rName (rAssos x))
    else map rName (rAssos x)

rNameClever x =
  let _name = rName x
  in  if head _name `elem` operators
        then "(I." ++ _name ++ ")"
        else "I."++_name

toClassExp :: RTerm -> String
toClassExp (RClass s nexpo t) = if nexpo
  then concat $["I.", s,"("] ++ intersperse ", " (map rNameClever t) ++ [")"]
    -- Typeclass itself is not exported http://stackoverflow.com/questions/17849870/closed-type-classes
  else intercalate ", " (map rNameClever t)

toOpExt :: RTerm -> Maybe String
toOpExt (RId n t) = Just $ concat ["(I.", n,")"]
toOpExt o = error $ show (shoCon o, rName o) -- (RDataCon{..}) = Nothing

-- asWarning $ putStrLn $ concat
--   ["  warn: (",rName, ") as a type constructor is not yet supported"]
-- return Nothing



-- liftIO $ mapM_ (putStrLn.(" -> "++).toS) declsF

-- data G = G1 Int
-- pattern JsG1 <- G1
-- mkG1 = G1

-- whenValid "" = error "prefix can't be empty"
-- whenValid x =
--   if all (`elem` ('_':['a'..'z']++['A'..'Z'])) x
--     then id
--     else error "invalid chars"
