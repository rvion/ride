 -- generated by rvion/jetpack-gen 
module Data.Text.AsT where
import Data.Text

-- ($c==) doesn't seem to be exported
-- ($fBinaryText) doesn't seem to be exported
-- ($fBinaryText1) doesn't seem to be exported
-- ($fBinaryText_$cget) doesn't seem to be exported
-- ($fBinaryText_$cput) doesn't seem to be exported
-- ($fDataText) doesn't seem to be exported
-- ($fDataText1) doesn't seem to be exported
-- ($fDataText10) doesn't seem to be exported
-- ($fDataText11) doesn't seem to be exported
-- ($fDataText12) doesn't seem to be exported
-- ($fDataText13) doesn't seem to be exported
-- ($fDataText14) doesn't seem to be exported
-- ($fDataText15) doesn't seem to be exported
-- ($fDataText16) doesn't seem to be exported
-- ($fDataText17) doesn't seem to be exported
-- ($fDataText2) doesn't seem to be exported
-- ($fDataText3) doesn't seem to be exported
-- ($fDataText4) doesn't seem to be exported
-- ($fDataText5) doesn't seem to be exported
-- ($fDataText6) doesn't seem to be exported
-- ($fDataText7) doesn't seem to be exported
-- ($fDataText8) doesn't seem to be exported
-- ($fDataText9) doesn't seem to be exported
-- ($fDataText_$cdataTypeOf) doesn't seem to be exported
-- ($fDataText_$cgfoldl) doesn't seem to be exported
-- ($fDataText_$cgmapM) doesn't seem to be exported
-- ($fDataText_$cgmapMo) doesn't seem to be exported
-- ($fDataText_$cgmapMp) doesn't seem to be exported
-- ($fDataText_$cgmapQ) doesn't seem to be exported
-- ($fDataText_$cgmapQi) doesn't seem to be exported
-- ($fDataText_$cgmapQl) doesn't seem to be exported
-- ($fDataText_$cgmapQr) doesn't seem to be exported
-- ($fDataText_$cgmapT) doesn't seem to be exported
-- ($fDataText_$cgunfold) doesn't seem to be exported
-- ($fDataText_$ctoConstr) doesn't seem to be exported
-- ($fDataText_$s$dmdataCast1) doesn't seem to be exported
-- ($fDataText_$s$dmdataCast2) doesn't seem to be exported
-- ($fDataText_$s$fData[]) doesn't seem to be exported
-- ($fDataText_$s$fData[]_$cdataCast1) doesn't seem to be exported
-- ($fDataText_$s$fData[]_$cdataCast2) doesn't seem to be exported
-- ($fDataText_$s$fData[]_$cdataTypeOf) doesn't seem to be exported
-- ($fDataText_$s$fData[]_$ctoConstr) doesn't seem to be exported
-- ($fDataText_w1) doesn't seem to be exported
-- ($fDataText_wild) doesn't seem to be exported
-- ($fDataText_wild1) doesn't seem to be exported
-- ($fDataText_ww1) doesn't seem to be exported
-- ($fDataText_ww2) doesn't seem to be exported
-- ($fDataText_ww3) doesn't seem to be exported
-- ($fDataText_ww4) doesn't seem to be exported
-- ($fDataText_ww5) doesn't seem to be exported
-- ($fDataText_ww6) doesn't seem to be exported
-- ($fDataText_z) doesn't seem to be exported
-- ($fEqText) doesn't seem to be exported
-- ($fEqText_$c/=) doesn't seem to be exported
-- ($fIsListText) doesn't seem to be exported
-- ($fIsListText1) doesn't seem to be exported
-- ($fIsListText_$cfromListN) doesn't seem to be exported
-- ($fIsStringText) doesn't seem to be exported
-- ($fMonoidText) doesn't seem to be exported
-- ($fNFDataText) doesn't seem to be exported
-- ($fNFDataText_$crnf) doesn't seem to be exported
-- ($fOrdText) doesn't seem to be exported
-- ($fOrdText_$c<) doesn't seem to be exported
-- ($fOrdText_$c<=) doesn't seem to be exported
-- ($fOrdText_$c>) doesn't seem to be exported
-- ($fOrdText_$c>=) doesn't seem to be exported
-- ($fOrdText_$ccompare) doesn't seem to be exported
-- ($fOrdText_$cmax) doesn't seem to be exported
-- ($fOrdText_$cmin) doesn't seem to be exported
-- ($fReadText) doesn't seem to be exported
-- ($fReadText1) doesn't seem to be exported
-- ($fReadText2) doesn't seem to be exported
-- ($fReadText3) doesn't seem to be exported
-- ($fReadText4) doesn't seem to be exported
-- ($fReadText_$creadList) doesn't seem to be exported
-- ($fReadText_$creadListPrec) doesn't seem to be exported
-- ($fReadText_$creadPrec) doesn't seem to be exported
-- ($fReadText_$creadsPrec) doesn't seem to be exported
-- ($fReadText_go) doesn't seem to be exported
-- ($w$ccompare) doesn't seem to be exported
-- ($w$cgmapQ) doesn't seem to be exported
-- ($w$cgmapQi) doesn't seem to be exported
-- ($w$cgunfold) doesn't seem to be exported
-- ($w$creadsPrec) doesn't seem to be exported
-- ($wa) doesn't seem to be exported
-- ($wcommonPrefixes) doesn't seem to be exported
-- ($wcopy) doesn't seem to be exported
-- ($wdropEnd) doesn't seem to be exported
-- ($wgo) doesn't seem to be exported
-- ($wgroupBy) doesn't seem to be exported
-- ($winits) doesn't seem to be exported
-- ($witerN) doesn't seem to be exported
-- ($witerNEnd) doesn't seem to be exported
-- ($wlines) doesn't seem to be exported
-- ($wreplace) doesn't seem to be exported
-- ($wstripPrefix) doesn't seem to be exported
-- ($wstripSuffix) doesn't seem to be exported
-- ($wtails) doesn't seem to be exported
-- ($wtakeEnd) doesn't seem to be exported
-- (TFCo:R:ItemText) doesn't seem to be exported

t_all :: (Char -> Bool) -> Text -> Bool
t_all =  T.all

t_any :: (Char -> Bool) -> Text -> Bool
t_any =  T.any

t_append :: Text -> Text -> Text
t_append =  T.append

t_break :: (Char -> Bool) -> Text -> (Text, Text)
t_break =  T.break

t_breakOn :: Text -> Text -> (Text, Text)
t_breakOn =  T.breakOn

t_breakOnAll :: Text -> Text -> [(Text, Text)]
t_breakOnAll =  T.breakOnAll

t_breakOnEnd :: Text -> Text -> (Text, Text)
t_breakOnEnd =  T.breakOnEnd

t_center :: Int -> Char -> Text -> Text
t_center =  T.center

t_chunksOf :: Int -> Text -> [Text]
t_chunksOf =  T.chunksOf

t_commonPrefixes :: Text -> Text -> Maybe (Text, Text, Text)
t_commonPrefixes =  T.commonPrefixes

t_compareLength :: Text -> Int -> Ordering
t_compareLength =  T.compareLength

t_concat :: [Text] -> Text
t_concat =  T.concat
-- (concat1) doesn't seem to be exported

t_concatMap :: (Char -> Text) -> Text -> Text
t_concatMap =  T.concatMap

t_cons :: Char -> Text -> Text
t_cons =  T.cons

t_copy :: Text -> Text
t_copy =  T.copy

t_count :: Text -> Text -> Int
t_count =  T.count
-- (count1) doesn't seem to be exported
-- (countChar) doesn't seem to be exported

t_drop :: Int -> Text -> Text
t_drop =  T.drop

t_dropAround :: (Char -> Bool) -> Text -> Text
t_dropAround =  T.dropAround

t_dropEnd :: Int -> Text -> Text
t_dropEnd =  T.dropEnd

t_dropWhile :: (Char -> Bool) -> Text -> Text
t_dropWhile =  T.dropWhile

t_dropWhileEnd :: (Char -> Bool) -> Text -> Text
t_dropWhileEnd =  T.dropWhileEnd
-- (emptyError) doesn't seem to be exported

t_filter :: (Char -> Bool) -> Text -> Text
t_filter =  T.filter

t_find :: (Char -> Bool) -> Text -> Maybe Char
t_find =  T.find

t_findIndex :: (Char -> Bool) -> Text -> Maybe Int
t_findIndex =  T.findIndex

t_foldl :: forall a. (a -> Char -> a) -> a -> Text -> a
t_foldl =  T.foldl

t_foldl' :: forall a. (a -> Char -> a) -> a -> Text -> a
t_foldl' =  T.foldl'

t_foldl1 :: (Char -> Char -> Char) -> Text -> Char
t_foldl1 =  T.foldl1

t_foldl1' :: (Char -> Char -> Char) -> Text -> Char
t_foldl1' =  T.foldl1'

t_foldr :: forall a. (Char -> a -> a) -> a -> Text -> a
t_foldr =  T.foldr

t_foldr1 :: (Char -> Char -> Char) -> Text -> Char
t_foldr1 =  T.foldr1

t_group :: Text -> [Text]
t_group =  T.group

t_groupBy :: (Char -> Char -> Bool) -> Text -> [Text]
t_groupBy =  T.groupBy

t_head :: Text -> Char
t_head =  T.head

t_index :: Text -> Int -> Char
t_index =  T.index

t_init :: Text -> Text
t_init =  T.init

t_inits :: Text -> [Text]
t_inits =  T.inits

t_intercalate :: Text -> [Text] -> Text
t_intercalate =  T.intercalate

t_intersperse :: Char -> Text -> Text
t_intersperse =  T.intersperse

t_isInfixOf :: Text -> Text -> Bool
t_isInfixOf =  T.isInfixOf

t_isPrefixOf :: Text -> Text -> Bool
t_isPrefixOf =  T.isPrefixOf

t_isSuffixOf :: Text -> Text -> Bool
t_isSuffixOf =  T.isSuffixOf
-- (iterN) doesn't seem to be exported

t_justifyLeft :: Int -> Char -> Text -> Text
t_justifyLeft =  T.justifyLeft

t_justifyRight :: Int -> Char -> Text -> Text
t_justifyRight =  T.justifyRight

t_last :: Text -> Char
t_last =  T.last

t_length :: Text -> Int
t_length =  T.length

t_lines :: Text -> [Text]
t_lines =  T.lines

t_map :: (Char -> Char) -> Text -> Text
t_map =  T.map

t_mapAccumL :: forall a. (a -> Char -> (a, Char)) -> a -> Text -> (a, Text)
t_mapAccumL =  T.mapAccumL

t_mapAccumR :: forall a. (a -> Char -> (a, Char)) -> a -> Text -> (a, Text)
t_mapAccumR =  T.mapAccumR

t_maximum :: Text -> Char
t_maximum =  T.maximum

t_minimum :: Text -> Char
t_minimum =  T.minimum

t_null :: Text -> Bool
t_null =  T.null

t_pack :: String -> Text
t_pack =  T.pack
-- (packConstr) doesn't seem to be exported

t_partition :: (Char -> Bool) -> Text -> (Text, Text)
t_partition =  T.partition

t_replace :: Text -> Text -> Text -> Text
t_replace =  T.replace

t_replicate :: Int -> Text -> Text
t_replicate =  T.replicate
-- (replicateChar) doesn't seem to be exported

t_reverse :: Text -> Text
t_reverse =  T.reverse

t_scanl :: (Char -> Char -> Char) -> Char -> Text -> Text
t_scanl =  T.scanl

t_scanl1 :: (Char -> Char -> Char) -> Text -> Text
t_scanl1 =  T.scanl1

t_scanr :: (Char -> Char -> Char) -> Char -> Text -> Text
t_scanr =  T.scanr

t_scanr1 :: (Char -> Char -> Char) -> Text -> Text
t_scanr1 =  T.scanr1
-- (second) doesn't seem to be exported

t_snoc :: Text -> Char -> Text
t_snoc =  T.snoc

t_span :: (Char -> Bool) -> Text -> (Text, Text)
t_span =  T.span

t_split :: (Char -> Bool) -> Text -> [Text]
t_split =  T.split

t_splitAt :: Int -> Text -> (Text, Text)
t_splitAt =  T.splitAt

t_splitOn :: Text -> Text -> [Text]
t_splitOn =  T.splitOn

t_strip :: Text -> Text
t_strip =  T.strip

t_stripEnd :: Text -> Text
t_stripEnd =  T.stripEnd

t_stripPrefix :: Text -> Text -> Maybe Text
t_stripPrefix =  T.stripPrefix

t_stripStart :: Text -> Text
t_stripStart =  T.stripStart

t_stripSuffix :: Text -> Text -> Maybe Text
t_stripSuffix =  T.stripSuffix

t_tail :: Text -> Text
t_tail =  T.tail

t_tails :: Text -> [Text]
t_tails =  T.tails

t_take :: Int -> Text -> Text
t_take =  T.take

t_takeEnd :: Int -> Text -> Text
t_takeEnd =  T.takeEnd

t_takeWhile :: (Char -> Bool) -> Text -> Text
t_takeWhile =  T.takeWhile
-- (textDataType) doesn't seem to be exported

t_toCaseFold :: Text -> Text
t_toCaseFold =  T.toCaseFold

t_toLower :: Text -> Text
t_toLower =  T.toLower

t_toTitle :: Text -> Text
t_toTitle =  T.toTitle

t_toUpper :: Text -> Text
t_toUpper =  T.toUpper

t_transpose :: [Text] -> [Text]
t_transpose =  T.transpose

t_uncons :: Text -> Maybe (Char, Text)
t_uncons =  T.uncons

t_unfoldr :: forall a. (a -> Maybe (Char, a)) -> a -> Text
t_unfoldr =  T.unfoldr

t_unfoldrN :: forall a. Int -> (a -> Maybe (Char, a)) -> a -> Text
t_unfoldrN =  T.unfoldrN

t_unlines :: [Text] -> Text
t_unlines =  T.unlines

t_unwords :: [Text] -> Text
t_unwords =  T.unwords

t_words :: Text -> [Text]
t_words =  T.words

t_zip :: Text -> Text -> [(Char, Char)]
t_zip =  T.zip

t_zipWith :: (Char -> Char -> Char) -> Text -> Text -> Text
t_zipWith =  T.zipWith
