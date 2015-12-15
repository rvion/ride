module Options.Applicative.Extra.AsOpt where
-- generated by https://github.com/rvion/ride/tree/master/jetpack-gen

import qualified Options.Applicative.Extra as I


-- opt_customExecParser :: forall a. ParserPrefs -> ParserInfo a -> IO a
opt_customExecParser = I.customExecParser

-- opt_execParser :: forall a. ParserInfo a -> IO a
opt_execParser = I.execParser

-- opt_execParserPure :: forall a. ParserPrefs -> ParserInfo a -> [String] -> ParserResult a
opt_execParserPure = I.execParserPure

-- opt_getParseResult :: forall a. ParserResult a -> Maybe a
opt_getParseResult = I.getParseResult

-- opt_handleParseResult :: forall a. ParserResult a -> IO a
opt_handleParseResult = I.handleParseResult

-- opt_helper :: forall a. Parser (a -> a)
opt_helper = I.helper

-- opt_hsubparser :: forall a. Mod CommandFields a -> Parser a
opt_hsubparser = I.hsubparser

-- opt_parserFailure :: forall a. ParserPrefs -> ParserInfo a -> ParseError -> Context -> ParserFailure ParserHelp
opt_parserFailure = I.parserFailure

-- opt_renderFailure :: ParserFailure ParserHelp -> String -> (String, ExitCode)
opt_renderFailure = I.renderFailure

-- opt_overFailure :: forall a. (ParserHelp -> ParserHelp) -> ParserResult a -> ParserResult a
opt_overFailure = I.overFailure

type OptCompletionResult  = I.CompletionResult

type OptParserFailure a = I.ParserFailure a

type OptParserResult a = I.ParserResult a

-- constructor :: a -> Success
opt_mk'Success =  I.Success
pattern OptSuccess a <-  I.Success a

-- constructor :: ParserFailure ParserHelp -> Failure
opt_mk'Failure =  I.Failure
pattern OptFailure a <-  I.Failure a

-- constructor :: CompletionResult -> CompletionInvoked
opt_mk'CompletionInvoked =  I.CompletionInvoked
pattern OptCompletionInvoked a <-  I.CompletionInvoked a
