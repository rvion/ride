{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}

module Text.Megaparsec.AsMp where
-- generated by rvion/jetpack-gen 

import Text.Megaparsec as I

mp_optional = I.optional
mp_alphaNumChar = I.alphaNumChar
mp_anyChar = I.anyChar
mp_asciiChar = I.asciiChar
mp_char = I.char
mp_char' = I.char'
mp_charCategory = I.charCategory
mp_controlChar = I.controlChar
mp_crlf = I.crlf
mp_digitChar = I.digitChar
mp_eol = I.eol
mp_hexDigitChar = I.hexDigitChar
mp_latin1Char = I.latin1Char
mp_letterChar = I.letterChar
mp_lowerChar = I.lowerChar
mp_markChar = I.markChar
mp_newline = I.newline
mp_noneOf = I.noneOf
mp_noneOf' = I.noneOf'
mp_numberChar = I.numberChar
mp_octDigitChar = I.octDigitChar
mp_oneOf = I.oneOf
mp_oneOf' = I.oneOf'
mp_printChar = I.printChar
mp_punctuationChar = I.punctuationChar
mp_satisfy = I.satisfy
mp_separatorChar = I.separatorChar
mp_space = I.space
mp_spaceChar = I.spaceChar
mp_string = I.string
mp_string' = I.string'
mp_symbolChar = I.symbolChar
mp_tab = I.tab
mp_upperChar = I.upperChar
mp_between = I.between
mp_choice = I.choice
mp_count = I.count
mp_count' = I.count'
mp_endBy = I.endBy
mp_endBy1 = I.endBy1
mp_manyTill = I.manyTill
mp_option = I.option
mp_sepBy = I.sepBy
mp_sepBy1 = I.sepBy1
mp_sepEndBy = I.sepEndBy
mp_sepEndBy1 = I.sepEndBy1
mp_skipMany = I.skipMany
mp_skipSome = I.skipSome
mp_someTill = I.someTill
mp_badMessage = I.badMessage
mp_errorIsUnknown = I.errorIsUnknown
mp_messageString = I.messageString
(<?>) = (I.<?>)
mp_getInput = I.getInput
mp_getPosition = I.getPosition
mp_getTabWidth = I.getTabWidth
mp_parse = I.parse
mp_parseFromFile = I.parseFromFile
mp_parseMaybe = I.parseMaybe
mp_parseTest = I.parseTest
mp_runParser = I.runParser
mp_runParser' = I.runParser'
mp_runParserT = I.runParserT
mp_runParserT' = I.runParserT'
mp_setInput = I.setInput
mp_setParserState = I.setParserState
mp_setPosition = I.setPosition
mp_setTabWidth = I.setTabWidth
mp_unexpected = I.unexpected
