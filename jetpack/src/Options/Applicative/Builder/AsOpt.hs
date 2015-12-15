module Options.Applicative.Builder.AsOpt where
-- generated by https://github.com/rvion/ride/tree/master/jetpack-gen

import qualified Options.Applicative.Builder as I

-- (<>) :: forall m. Monoid m => m -> m -> m
(<>) = (I.<>)

-- opt_abortOption :: forall a. ParseError -> Mod OptionFields (a -> a) -> Parser (a -> a)
opt_abortOption = I.abortOption

-- opt_action :: forall (f :: * -> *) a. HasCompleter f => String -> Mod f a
opt_action = I.action

-- opt_argument :: forall a. ReadM a -> Mod ArgumentFields a -> Parser a
opt_argument = I.argument

-- opt_auto :: forall a. Read a => ReadM a
opt_auto = I.auto

-- opt_briefDesc :: forall a. InfoMod a
opt_briefDesc = I.briefDesc

-- opt_columns :: Int -> PrefsMod
opt_columns = I.columns

-- opt_command :: forall a. String -> ParserInfo a -> Mod CommandFields a
opt_command = I.command

-- opt_completeWith :: forall (f :: * -> *) a. HasCompleter f => [String] -> Mod f a
opt_completeWith = I.completeWith

-- opt_completer :: forall (f :: * -> *) a. HasCompleter f => Completer -> Mod f a
opt_completer = I.completer

-- opt_disabled :: forall a. ReadM a
opt_disabled = I.disabled

-- opt_disambiguate :: PrefsMod
opt_disambiguate = I.disambiguate

-- opt_eitherReader :: forall a. (String -> Either String a) -> ReadM a
opt_eitherReader = I.eitherReader

-- opt_failureCode :: forall a. Int -> InfoMod a
opt_failureCode = I.failureCode

-- opt_flag :: forall a. a -> a -> Mod FlagFields a -> Parser a
opt_flag = I.flag

-- opt_flag' :: forall a. a -> Mod FlagFields a -> Parser a
opt_flag' = I.flag'

-- opt_footer :: forall a. String -> InfoMod a
opt_footer = I.footer

-- opt_footerDoc :: forall a. Maybe Doc -> InfoMod a
opt_footerDoc = I.footerDoc

-- opt_fullDesc :: forall a. InfoMod a
opt_fullDesc = I.fullDesc

-- opt_header :: forall a. String -> InfoMod a
opt_header = I.header

-- opt_headerDoc :: forall a. Maybe Doc -> InfoMod a
opt_headerDoc = I.headerDoc

-- opt_help :: forall (f :: * -> *) a. String -> Mod f a
opt_help = I.help

-- opt_helpDoc :: forall (f :: * -> *) a. Maybe Doc -> Mod f a
opt_helpDoc = I.helpDoc

-- opt_hidden :: forall (f :: * -> *) a. Mod f a
opt_hidden = I.hidden

-- opt_idm :: forall m. Monoid m => m
opt_idm = I.idm

-- opt_info :: forall a. Parser a -> InfoMod a -> ParserInfo a
opt_info = I.info

-- opt_infoOption :: forall a. String -> Mod OptionFields (a -> a) -> Parser (a -> a)
opt_infoOption = I.infoOption

-- opt_long :: forall (f :: * -> *) a. HasName f => String -> Mod f a
opt_long = I.long

-- opt_metavar :: forall (f :: * -> *) a. HasMetavar f => String -> Mod f a
opt_metavar = I.metavar

-- opt_multiSuffix :: String -> PrefsMod
opt_multiSuffix = I.multiSuffix

-- opt_noArgError :: forall a. ParseError -> Mod OptionFields a
opt_noArgError = I.noArgError

-- opt_noBacktrack :: PrefsMod
opt_noBacktrack = I.noBacktrack

-- opt_noIntersperse :: forall a. InfoMod a
opt_noIntersperse = I.noIntersperse

-- opt_option :: forall a. ReadM a -> Mod OptionFields a -> Parser a
opt_option = I.option

-- opt_prefs :: PrefsMod -> ParserPrefs
opt_prefs = I.prefs

-- opt_progDesc :: forall a. String -> InfoMod a
opt_progDesc = I.progDesc

-- opt_progDescDoc :: forall a. Maybe Doc -> InfoMod a
opt_progDescDoc = I.progDescDoc

-- opt_short :: forall (f :: * -> *) a. HasName f => Char -> Mod f a
opt_short = I.short

-- opt_showDefault :: forall (f :: * -> *) a. Show a => Mod f a
opt_showDefault = I.showDefault

-- opt_showDefaultWith :: forall a (f :: * -> *). (a -> String) -> Mod f a
opt_showDefaultWith = I.showDefaultWith

-- opt_showHelpOnError :: PrefsMod
opt_showHelpOnError = I.showHelpOnError

-- opt_str :: ReadM String
opt_str = I.str

-- opt_strArgument :: Mod ArgumentFields String -> Parser String
opt_strArgument = I.strArgument

-- opt_strOption :: Mod OptionFields String -> Parser String
opt_strOption = I.strOption

-- opt_subparser :: forall a. Mod CommandFields a -> Parser a
opt_subparser = I.subparser

-- opt_switch :: Mod FlagFields Bool -> Parser Bool
opt_switch = I.switch

-- opt_value :: forall a (f :: * -> *). HasValue f => a -> Mod f a
opt_value = I.value

-- opt_internal :: forall (f :: * -> *) a. Mod f a
opt_internal = I.internal

-- opt_readerAbort :: forall a. ParseError -> ReadM a
opt_readerAbort = I.readerAbort

-- opt_readerError :: forall a. String -> ReadM a
opt_readerError = I.readerError

-- opt_mappend :: a -> a -> a
opt_mappend = I.mappend

type OptInfoMod a = I.InfoMod a
type OptPrefsMod  = I.PrefsMod
type OptArgumentFields a = I.ArgumentFields a
type OptCommandFields a = I.CommandFields a
type OptFlagFields a = I.FlagFields a
type OptMod a b = I.Mod a b
type OptOptionFields a = I.OptionFields a
type OptParseError  = I.ParseError
type OptReadM a = I.ReadM a