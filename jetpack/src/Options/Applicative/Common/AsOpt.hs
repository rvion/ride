module Options.Applicative.Common.AsOpt
  ( module Options.Applicative.Common.AsOpt
  ) where
-- generated by https://github.com/rvion/ride/tree/master/jetpack-gen

import qualified Options.Applicative.Common as I


-- opt_evalParser :: forall a. Parser a -> Maybe a
opt_evalParser = I.evalParser

-- opt_liftOpt :: forall a. Option a -> Parser a
opt_liftOpt = I.liftOpt

-- opt_mapParser :: forall a b. (forall x. OptHelpInfo -> Option x -> b) -> Parser a -> [b]
opt_mapParser = I.mapParser

-- opt_optionNames :: forall a. OptReader a -> [OptName]
opt_optionNames = I.optionNames

-- opt_runParser :: forall a (m :: * -> *). MonadP m => ArgPolicy -> Parser a -> Args -> m (a, Args)
opt_runParser = I.runParser

-- opt_runParserFully :: forall a (m :: * -> *). MonadP m => ArgPolicy -> Parser a -> Args -> m a
opt_runParserFully = I.runParserFully

-- opt_runParserInfo :: forall a (m :: * -> *). MonadP m => ParserInfo a -> Args -> m a
opt_runParserInfo = I.runParserInfo

-- opt_showOption :: OptName -> String
opt_showOption = I.showOption

-- opt_treeMapParser :: forall a b. (forall x. OptHelpInfo -> Option x -> b) -> Parser a -> OptTree b
opt_treeMapParser = I.treeMapParser

type OptParser a = I.Parser a

type OptParserInfo a = I.ParserInfo a
get_opt_infoParser o = I.infoParser o
set_opt_infoParser x o = o { I.infoParser = x}
get_opt_infoFullDesc o = I.infoFullDesc o
set_opt_infoFullDesc x o = o { I.infoFullDesc = x}
get_opt_infoProgDesc o = I.infoProgDesc o
set_opt_infoProgDesc x o = o { I.infoProgDesc = x}
get_opt_infoHeader o = I.infoHeader o
set_opt_infoHeader x o = o { I.infoHeader = x}
get_opt_infoFooter o = I.infoFooter o
set_opt_infoFooter x o = o { I.infoFooter = x}
get_opt_infoFailureCode o = I.infoFailureCode o
set_opt_infoFailureCode x o = o { I.infoFailureCode = x}
get_opt_infoIntersperse o = I.infoIntersperse o
set_opt_infoIntersperse x o = o { I.infoIntersperse = x}

-- constructor :: Parser a -> Bool -> Chunk Doc -> Chunk Doc -> Chunk Doc -> Int -> Bool -> ParserInfo
opt_mk'ParserInfo =  I.ParserInfo
pattern OptParserInfo a b c d e f g <-  I.ParserInfo a b c d e f g

type OptParserPrefs  = I.ParserPrefs
get_opt_prefMultiSuffix o = I.prefMultiSuffix o
set_opt_prefMultiSuffix x o = o { I.prefMultiSuffix = x}
get_opt_prefDisambiguate o = I.prefDisambiguate o
set_opt_prefDisambiguate x o = o { I.prefDisambiguate = x}
get_opt_prefShowHelpOnError o = I.prefShowHelpOnError o
set_opt_prefShowHelpOnError x o = o { I.prefShowHelpOnError = x}
get_opt_prefBacktrack o = I.prefBacktrack o
set_opt_prefBacktrack x o = o { I.prefBacktrack = x}
get_opt_prefColumns o = I.prefColumns o
set_opt_prefColumns x o = o { I.prefColumns = x}

-- constructor :: String -> Bool -> Bool -> Bool -> Int -> ParserPrefs
opt_mk'ParserPrefs =  I.ParserPrefs
pattern OptParserPrefs a b c d e <-  I.ParserPrefs a b c d e
