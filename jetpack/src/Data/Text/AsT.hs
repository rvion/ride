module Data.Text.AsT
  ( module Data.Text.AsT
  ) where
-- generated by https://github.com/rvion/ride/tree/master/jetpack-gen

import qualified Data.Text as I


-- t_all :: (Char -> Bool) -> Text -> Bool
t_all = I.all

-- t_any :: (Char -> Bool) -> Text -> Bool
t_any = I.any

-- t_append :: Text -> Text -> Text
t_append = I.append

-- t_break :: (Char -> Bool) -> Text -> (Text, Text)
t_break = I.break

-- t_breakOn :: Text -> Text -> (Text, Text)
t_breakOn = I.breakOn

-- t_breakOnAll :: Text -> Text -> [(Text, Text)]
t_breakOnAll = I.breakOnAll

-- t_breakOnEnd :: Text -> Text -> (Text, Text)
t_breakOnEnd = I.breakOnEnd

-- t_center :: Int -> Char -> Text -> Text
t_center = I.center

-- t_chunksOf :: Int -> Text -> [Text]
t_chunksOf = I.chunksOf

-- t_commonPrefixes :: Text -> Text -> Maybe (Text, Text, Text)
t_commonPrefixes = I.commonPrefixes

-- t_compareLength :: Text -> Int -> Ordering
t_compareLength = I.compareLength

-- t_concat :: [Text] -> Text
t_concat = I.concat

-- t_concatMap :: (Char -> Text) -> Text -> Text
t_concatMap = I.concatMap

-- t_cons :: Char -> Text -> Text
t_cons = I.cons

-- t_copy :: Text -> Text
t_copy = I.copy

-- t_count :: Text -> Text -> Int
t_count = I.count

-- t_drop :: Int -> Text -> Text
t_drop = I.drop

-- t_dropAround :: (Char -> Bool) -> Text -> Text
t_dropAround = I.dropAround

-- t_dropEnd :: Int -> Text -> Text
t_dropEnd = I.dropEnd

-- t_dropWhile :: (Char -> Bool) -> Text -> Text
t_dropWhile = I.dropWhile

-- t_dropWhileEnd :: (Char -> Bool) -> Text -> Text
t_dropWhileEnd = I.dropWhileEnd

-- t_filter :: (Char -> Bool) -> Text -> Text
t_filter = I.filter

-- t_find :: (Char -> Bool) -> Text -> Maybe Char
t_find = I.find

-- t_findIndex :: (Char -> Bool) -> Text -> Maybe Int
t_findIndex = I.findIndex

-- t_foldl :: forall a. (a -> Char -> a) -> a -> Text -> a
t_foldl = I.foldl

-- t_foldl' :: forall a. (a -> Char -> a) -> a -> Text -> a
t_foldl' = I.foldl'

-- t_foldl1 :: (Char -> Char -> Char) -> Text -> Char
t_foldl1 = I.foldl1

-- t_foldl1' :: (Char -> Char -> Char) -> Text -> Char
t_foldl1' = I.foldl1'

-- t_foldr :: forall a. (Char -> a -> a) -> a -> Text -> a
t_foldr = I.foldr

-- t_foldr1 :: (Char -> Char -> Char) -> Text -> Char
t_foldr1 = I.foldr1

-- t_group :: Text -> [Text]
t_group = I.group

-- t_groupBy :: (Char -> Char -> Bool) -> Text -> [Text]
t_groupBy = I.groupBy

-- t_head :: Text -> Char
t_head = I.head

-- t_index :: Text -> Int -> Char
t_index = I.index

-- t_init :: Text -> Text
t_init = I.init

-- t_inits :: Text -> [Text]
t_inits = I.inits

-- t_intercalate :: Text -> [Text] -> Text
t_intercalate = I.intercalate

-- t_intersperse :: Char -> Text -> Text
t_intersperse = I.intersperse

-- t_isInfixOf :: Text -> Text -> Bool
t_isInfixOf = I.isInfixOf

-- t_isPrefixOf :: Text -> Text -> Bool
t_isPrefixOf = I.isPrefixOf

-- t_isSuffixOf :: Text -> Text -> Bool
t_isSuffixOf = I.isSuffixOf

-- t_justifyLeft :: Int -> Char -> Text -> Text
t_justifyLeft = I.justifyLeft

-- t_justifyRight :: Int -> Char -> Text -> Text
t_justifyRight = I.justifyRight

-- t_last :: Text -> Char
t_last = I.last

-- t_length :: Text -> Int
t_length = I.length

-- t_lines :: Text -> [Text]
t_lines = I.lines

-- t_map :: (Char -> Char) -> Text -> Text
t_map = I.map

-- t_mapAccumL :: forall a. (a -> Char -> (a, Char)) -> a -> Text -> (a, Text)
t_mapAccumL = I.mapAccumL

-- t_mapAccumR :: forall a. (a -> Char -> (a, Char)) -> a -> Text -> (a, Text)
t_mapAccumR = I.mapAccumR

-- t_maximum :: Text -> Char
t_maximum = I.maximum

-- t_minimum :: Text -> Char
t_minimum = I.minimum

-- t_null :: Text -> Bool
t_null = I.null

-- t_pack :: String -> Text
t_pack = I.pack

-- t_partition :: (Char -> Bool) -> Text -> (Text, Text)
t_partition = I.partition

-- t_replace :: Text -> Text -> Text -> Text
t_replace = I.replace

-- t_replicate :: Int -> Text -> Text
t_replicate = I.replicate

-- t_reverse :: Text -> Text
t_reverse = I.reverse

-- t_scanl :: (Char -> Char -> Char) -> Char -> Text -> Text
t_scanl = I.scanl

-- t_scanl1 :: (Char -> Char -> Char) -> Text -> Text
t_scanl1 = I.scanl1

-- t_scanr :: (Char -> Char -> Char) -> Char -> Text -> Text
t_scanr = I.scanr

-- t_scanr1 :: (Char -> Char -> Char) -> Text -> Text
t_scanr1 = I.scanr1

-- t_snoc :: Text -> Char -> Text
t_snoc = I.snoc

-- t_span :: (Char -> Bool) -> Text -> (Text, Text)
t_span = I.span

-- t_split :: (Char -> Bool) -> Text -> [Text]
t_split = I.split

-- t_splitAt :: Int -> Text -> (Text, Text)
t_splitAt = I.splitAt

-- t_splitOn :: Text -> Text -> [Text]
t_splitOn = I.splitOn

-- t_strip :: Text -> Text
t_strip = I.strip

-- t_stripEnd :: Text -> Text
t_stripEnd = I.stripEnd

-- t_stripPrefix :: Text -> Text -> Maybe Text
t_stripPrefix = I.stripPrefix

-- t_stripStart :: Text -> Text
t_stripStart = I.stripStart

-- t_stripSuffix :: Text -> Text -> Maybe Text
t_stripSuffix = I.stripSuffix

-- t_tail :: Text -> Text
t_tail = I.tail

-- t_tails :: Text -> [Text]
t_tails = I.tails

-- t_take :: Int -> Text -> Text
t_take = I.take

-- t_takeEnd :: Int -> Text -> Text
t_takeEnd = I.takeEnd

-- t_takeWhile :: (Char -> Bool) -> Text -> Text
t_takeWhile = I.takeWhile

-- t_toCaseFold :: Text -> Text
t_toCaseFold = I.toCaseFold

-- t_toLower :: Text -> Text
t_toLower = I.toLower

-- t_toTitle :: Text -> Text
t_toTitle = I.toTitle

-- t_toUpper :: Text -> Text
t_toUpper = I.toUpper

-- t_transpose :: [Text] -> [Text]
t_transpose = I.transpose

-- t_uncons :: Text -> Maybe (Char, Text)
t_uncons = I.uncons

-- t_unfoldr :: forall a. (a -> Maybe (Char, a)) -> a -> Text
t_unfoldr = I.unfoldr

-- t_unfoldrN :: forall a. Int -> (a -> Maybe (Char, a)) -> a -> Text
t_unfoldrN = I.unfoldrN

-- t_unlines :: [Text] -> Text
t_unlines = I.unlines

-- t_unwords :: [Text] -> Text
t_unwords = I.unwords

-- t_words :: Text -> [Text]
t_words = I.words

-- t_zip :: Text -> Text -> [(Char, Char)]
t_zip = I.zip

-- t_zipWith :: (Char -> Char -> Char) -> Text -> Text -> Text
t_zipWith = I.zipWith

-- t_empty :: Text
t_empty = I.empty

-- t_singleton :: Char -> Text
t_singleton = I.singleton

-- t_unpack :: Text -> String
t_unpack = I.unpack

-- t_unpackCString# :: Addr# -> Text
t_unpackCString# = I.unpackCString#

type TText  = I.Text
