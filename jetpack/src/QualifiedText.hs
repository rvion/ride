module QualifiedText where

import Data.Text as Exports (Text)
import qualified Data.Text as T

text_pack:: String -> Text
text_pack = T.pack

text_unpack:: Text -> String
text_unpack = T.unpack

text_singleton:: Char -> Text
text_singleton = T.singleton

text_empty:: Text
text_empty = T.empty

text_cons:: Char -> Text -> Text
text_cons = T.cons

text_snoc:: Text -> Char -> Text
text_snoc = T.snoc

text_append:: Text -> Text -> Text
text_append = T.append

text_uncons:: Text -> Maybe (Char, Text)
text_uncons = T.uncons

text_head:: Text -> Char
text_head = T.head

text_last:: Text -> Char
text_last = T.last

text_tail:: Text -> Text
text_tail = T.tail

text_init:: Text -> Text
text_init = T.init

text_null:: Text -> Bool
text_null = T.null

text_length:: Text -> Int
text_length = T.length

text_compareLength:: Text -> Int -> Ordering
text_compareLength = T.compareLength

text_map:: (Char -> Char) -> Text -> Text
text_map = T.map

text_intercalate:: Text -> [Text] -> Text
text_intercalate = T.intercalate

text_intersperse:: Char -> Text -> Text
text_intersperse = T.intersperse

text_transpose:: [Text] -> [Text]
text_transpose = T.transpose

text_reverse:: Text -> Text
text_reverse = T.reverse

text_replace:: Text -> Text -> Text -> Text
text_replace = T.replace

text_toCaseFold:: Text -> Text
text_toCaseFold = T.toCaseFold

text_toLower:: Text -> Text
text_toLower = T.toLower

text_toUpper:: Text -> Text
text_toUpper = T.toUpper

text_toTitle:: Text -> Text
text_toTitle = T.toTitle

text_justifyLeft:: Int -> Char -> Text -> Text
text_justifyLeft = T.justifyLeft

text_justifyRight:: Int -> Char -> Text -> Text
text_justifyRight = T.justifyRight

text_center:: Int -> Char -> Text -> Text
text_center = T.center

text_foldl:: (a -> Char -> a) -> a -> Text -> a
text_foldl = T.foldl

text_foldl' :: (a -> Char -> a) -> a -> Text -> a
text_foldl' = T.foldl'

text_foldl1:: (Char -> Char -> Char) -> Text -> Char
text_foldl1 = T.foldl1

text_foldl1':: (Char -> Char -> Char) -> Text -> Char
text_foldl1' = T.foldl1'

text_foldr:: (Char -> a -> a) -> a -> Text -> a
text_foldr = T.foldr

text_foldr1:: (Char -> Char -> Char) -> Text -> Char
text_foldr1 = T.foldr1

text_concat:: [Text] -> Text
text_concat = T.concat

text_concatMap:: (Char -> Text) -> Text -> Text
text_concatMap = T.concatMap

text_any:: (Char -> Bool) -> Text -> Bool
text_any = T.any

text_all:: (Char -> Bool) -> Text -> Bool
text_all = T.all

text_maximum:: Text -> Char
text_maximum = T.maximum

text_minimum:: Text -> Char
text_minimum = T.minimum

text_scanl:: (Char -> Char -> Char) -> Char -> Text -> Text
text_scanl = T.scanl

text_scanl1:: (Char -> Char -> Char) -> Text -> Text
text_scanl1 = T.scanl1

text_scanr:: (Char -> Char -> Char) -> Char -> Text -> Text
text_scanr = T.scanr

text_scanr1:: (Char -> Char -> Char) -> Text -> Text
text_scanr1 = T.scanr1

text_mapAccumL:: (a -> Char -> (a, Char)) -> a -> Text -> (a, Text)
text_mapAccumL = T.mapAccumL

text_mapAccumR:: (a -> Char -> (a, Char)) -> a -> Text -> (a, Text)
text_mapAccumR = T.mapAccumR

text_replicate:: Int -> Text -> Text
text_replicate = T.replicate

text_unfoldr:: (a -> Maybe (Char, a)) -> a -> Text
text_unfoldr = T.unfoldr

text_unfoldrN:: Int -> (a -> Maybe (Char, a)) -> a -> Text
text_unfoldrN = T.unfoldrN

text_take:: Int -> Text -> Text
text_take = T.take

text_takeEnd:: Int -> Text -> Text
text_takeEnd = T.takeEnd

text_drop:: Int -> Text -> Text
text_drop = T.drop

text_dropEnd:: Int -> Text -> Text
text_dropEnd = T.dropEnd

text_takeWhile:: (Char -> Bool) -> Text -> Text
text_takeWhile = T.takeWhile

text_dropWhile:: (Char -> Bool) -> Text -> Text
text_dropWhile = T.dropWhile

text_dropWhileEnd:: (Char -> Bool) -> Text -> Text
text_dropWhileEnd = T.dropWhileEnd

text_dropAround:: (Char -> Bool) -> Text -> Text
text_dropAround = T.dropAround

text_strip:: Text -> Text
text_strip = T.strip

text_stripStart:: Text -> Text
text_stripStart = T.stripStart

text_stripEnd:: Text -> Text
text_stripEnd = T.stripEnd

text_splitAt:: Int -> Text -> (Text, Text)
text_splitAt = T.splitAt

text_breakOn:: Text -> Text -> (Text, Text)
text_breakOn = T.breakOn

text_breakOnEnd:: Text -> Text -> (Text, Text)
text_breakOnEnd = T.breakOnEnd

text_break:: (Char -> Bool) -> Text -> (Text, Text)
text_break = T.break

text_span:: (Char -> Bool) -> Text -> (Text, Text)
text_span = T.span

text_group:: Text -> [Text]
text_group = T.group

text_groupBy:: (Char -> Char -> Bool) -> Text -> [Text]
text_groupBy = T.groupBy

text_inits:: Text -> [Text]
text_inits = T.inits

text_tails:: Text -> [Text]
text_tails = T.tails

text_splitOn:: Text -> Text -> [Text]
text_splitOn = T.splitOn

text_split:: (Char -> Bool) -> Text -> [Text]
text_split = T.split

text_chunksOf:: Int -> Text -> [Text]
text_chunksOf = T.chunksOf

text_lines:: Text -> [Text]
text_lines = T.lines

text_words:: Text -> [Text]
text_words = T.words

text_unlines:: [Text] -> Text
text_unlines = T.unlines

text_unwords:: [Text] -> Text
text_unwords = T.unwords

text_isPrefixOf:: Text -> Text -> Bool
text_isPrefixOf = T.isPrefixOf

text_isSuffixOf:: Text -> Text -> Bool
text_isSuffixOf = T.isSuffixOf

text_isInfixOf:: Text -> Text -> Bool
text_isInfixOf = T.isInfixOf

text_stripPrefix:: Text -> Text -> Maybe Text
text_stripPrefix = T.stripPrefix

text_stripSuffix:: Text -> Text -> Maybe Text
text_stripSuffix = T.stripSuffix

text_commonPrefixes:: Text -> Text -> Maybe (Text, Text, Text)
text_commonPrefixes = T.commonPrefixes

text_filter:: (Char -> Bool) -> Text -> Text
text_filter = T.filter

text_breakOnAll:: Text -> Text -> [(Text, Text)]
text_breakOnAll = T.breakOnAll

text_find:: (Char -> Bool) -> Text -> Maybe Char
text_find = T.find

text_partition:: (Char -> Bool) -> Text -> (Text, Text)
text_partition = T.partition

text_index:: Text -> Int -> Char
text_index = T.index

text_findIndex:: (Char -> Bool) -> Text -> Maybe Int
text_findIndex = T.findIndex

text_count:: Text -> Text -> Int
text_count = T.count

text_zip:: Text -> Text -> [(Char, Char)]
text_zip = T.zip

text_zipWith:: (Char -> Char -> Char) -> Text -> Text -> Text
text_zipWith = T.zipWith

text_copy:: Text -> Text
text_copy = T.copy
-- text_unpackCString = T.unpackCString# :: Addr# -> Text
