```shell
*Gen> jetpackGen
loading package DB...
  OK (3 elems)
loading packages...
  OK (180 elems)
loading modules...
  OK (2282 elems)
loading reexports plan...
  OK (28 elems)
loading cabal deps...
  OK (17 elems)
reexporting.. Data.Map.Strict...
  OK (108 elems)
reexporting.. Data.Map...
  warn: (!) previously exported
  warn: (\\) previously exported
  OK (219 elems)
reexporting.. Data.Set...
  warn: (\\) previously exported
  OK (272 elems)
reexporting.. Data.HashMap.Strict...
  warn: (!) previously exported
  OK (304 elems)
reexporting.. Data.List.NonEmpty...
  OK (365 elems)
reexporting.. Data.Text...
  OK (459 elems)
reexporting.. Data.Text.Encoding...
  info: (decodeASCII) is not reexported because it is deprecated.
  OK (481 elems)
reexporting.. Data.Text.IO...
  OK (494 elems)
reexporting.. Data.Text.Lazy...
  OK (594 elems)
reexporting.. Data.Text.Lazy.Encoding...
  info: (decodeASCII) is not reexported because it is deprecated.
  OK (613 elems)
reexporting.. Data.Text.Lazy.IO...
  OK (625 elems)
reexporting.. Data.ByteString...
  info: (breakByte) is not reexported because it is deprecated.
  info: (findSubstring) is not reexported because it is deprecated.
  info: (findSubstrings) is not reexported because it is deprecated.
  info: (hPutStrLn) is not reexported because it is deprecated.
  info: (putStrLn) is not reexported because it is deprecated.
  OK (724 elems)
reexporting.. Data.ByteString.Char8...
  info: (findSubstring) is not reexported because it is deprecated.
  info: (findSubstrings) is not reexported because it is deprecated.
  warn: (bs_append) previously exported
  warn: (bs_breakSubstring) previously exported
  warn: (bs_concat) previously exported
  warn: (bs_copy) previously exported
  warn: (bs_drop) previously exported
  warn: (bs_empty) previously exported
  warn: (bs_getContents) previously exported
  warn: (bs_getLine) previously exported
  warn: (bs_group) previously exported
  warn: (bs_hGet) previously exported
  warn: (bs_hGetContents) previously exported
  warn: (bs_hGetLine) previously exported
  warn: (bs_hGetNonBlocking) previously exported
  warn: (bs_hGetSome) previously exported
  warn: (bs_hPut) previously exported
  warn: (bs_hPutNonBlocking) previously exported
  warn: (bs_hPutStr) previously exported
  warn: (bs_init) previously exported
  warn: (bs_inits) previously exported
  warn: (bs_interact) previously exported
  warn: (bs_intercalate) previously exported
  warn: (bs_isInfixOf) previously exported
  warn: (bs_isPrefixOf) previously exported
  warn: (bs_isSuffixOf) previously exported
  warn: (bs_length) previously exported
  warn: (bs_null) previously exported
  warn: (bs_packCString) previously exported
  warn: (bs_packCStringLen) previously exported
  warn: (bs_putStr) previously exported
  warn: (bs_reverse) previously exported
  warn: (bs_sort) previously exported
  warn: (bs_splitAt) previously exported
  warn: (bs_tail) previously exported
  warn: (bs_tails) previously exported
  warn: (bs_take) previously exported
  warn: (bs_transpose) previously exported
  warn: (bs_useAsCString) previously exported
  warn: (bs_useAsCStringLen) previously exported
  warn: (bs_all) previously exported
  warn: (bs_any) previously exported
  warn: (bs_appendFile) previously exported
  warn: (bs_break) previously exported
  warn: (bs_breakEnd) previously exported
  warn: (bs_concatMap) previously exported
  warn: (bs_cons) previously exported
  warn: (bs_count) previously exported
  warn: (bs_dropWhile) previously exported
  warn: (bs_elem) previously exported
  warn: (bs_elemIndex) previously exported
  warn: (bs_elemIndexEnd) previously exported
  warn: (bs_elemIndices) previously exported
  warn: (bs_filter) previously exported
  warn: (bs_find) previously exported
  warn: (bs_findIndex) previously exported
  warn: (bs_findIndices) previously exported
  warn: (bs_foldl) previously exported
  warn: (bs_foldl') previously exported
  warn: (bs_foldl1) previously exported
  warn: (bs_foldl1') previously exported
  warn: (bs_foldr) previously exported
  warn: (bs_foldr') previously exported
  warn: (bs_foldr1) previously exported
  warn: (bs_foldr1') previously exported
  warn: (bs_groupBy) previously exported
  warn: (bs_head) previously exported
  warn: (bs_index) previously exported
  warn: (bs_intersperse) previously exported
  warn: (bs_last) previously exported
  warn: (bs_map) previously exported
  warn: (bs_mapAccumL) previously exported
  warn: (bs_mapAccumR) previously exported
  warn: (bs_maximum) previously exported
  warn: (bs_minimum) previously exported
  warn: (bs_notElem) previously exported
  warn: (bs_pack) previously exported
  warn: (bs_readFile) previously exported
  warn: (bs_replicate) previously exported
  warn: (bs_scanl) previously exported
  warn: (bs_scanl1) previously exported
  warn: (bs_scanr) previously exported
  warn: (bs_scanr1) previously exported
  warn: (bs_singleton) previously exported
  warn: (bs_snoc) previously exported
  warn: (bs_span) previously exported
  warn: (bs_spanEnd) previously exported
  warn: (bs_split) previously exported
  warn: (bs_splitWith) previously exported
  warn: (bs_takeWhile) previously exported
  warn: (bs_uncons) previously exported
  warn: (bs_unfoldr) previously exported
  warn: (bs_unfoldrN) previously exported
  warn: (bs_unpack) previously exported
  warn: (bs_unsnoc) previously exported
  warn: (bs_unzip) previously exported
  warn: (bs_writeFile) previously exported
  warn: (bs_zip) previously exported
  warn: (bs_zipWith) previously exported
  warn: (bs_ByteString) previously exported
  OK (732 elems)
reexporting.. Data.ByteString.Lazy...
  info: (putStrLn) is not reexported because it is deprecated.
  OK (823 elems)
reexporting.. Data.ByteString.Lazy.Char8...
  warn: (lbs_append) previously exported
  warn: (lbs_concat) previously exported
  warn: (lbs_copy) previously exported
  warn: (lbs_cycle) previously exported
  warn: (lbs_drop) previously exported
  warn: (lbs_empty) previously exported
  warn: (lbs_fromChunks) previously exported
  warn: (lbs_fromStrict) previously exported
  warn: (lbs_getContents) previously exported
  warn: (lbs_group) previously exported
  warn: (lbs_hGet) previously exported
  warn: (lbs_hGetContents) previously exported
  warn: (lbs_hGetNonBlocking) previously exported
  warn: (lbs_hPut) previously exported
  warn: (lbs_hPutNonBlocking) previously exported
  warn: (lbs_hPutStr) previously exported
  warn: (lbs_init) previously exported
  warn: (lbs_inits) previously exported
  warn: (lbs_interact) previously exported
  warn: (lbs_intercalate) previously exported
  warn: (lbs_isPrefixOf) previously exported
  warn: (lbs_isSuffixOf) previously exported
  warn: (lbs_length) previously exported
  warn: (lbs_null) previously exported
  warn: (lbs_putStr) previously exported
  warn: (lbs_reverse) previously exported
  warn: (lbs_splitAt) previously exported
  warn: (lbs_tail) previously exported
  warn: (lbs_tails) previously exported
  warn: (lbs_take) previously exported
  warn: (lbs_toChunks) previously exported
  warn: (lbs_toStrict) previously exported
  warn: (lbs_transpose) previously exported
  warn: (lbs_all) previously exported
  warn: (lbs_any) previously exported
  warn: (lbs_appendFile) previously exported
  warn: (lbs_break) previously exported
  warn: (lbs_concatMap) previously exported
  warn: (lbs_cons) previously exported
  warn: (lbs_cons') previously exported
  warn: (lbs_count) previously exported
  warn: (lbs_dropWhile) previously exported
  warn: (lbs_elem) previously exported
  warn: (lbs_elemIndex) previously exported
  warn: (lbs_elemIndices) previously exported
  warn: (lbs_filter) previously exported
  warn: (lbs_find) previously exported
  warn: (lbs_findIndex) previously exported
  warn: (lbs_findIndices) previously exported
  warn: (lbs_foldl) previously exported
  warn: (lbs_foldl') previously exported
  warn: (lbs_foldl1) previously exported
  warn: (lbs_foldl1') previously exported
  warn: (lbs_foldr) previously exported
  warn: (lbs_foldr1) previously exported
  warn: (lbs_groupBy) previously exported
  warn: (lbs_head) previously exported
  warn: (lbs_index) previously exported
  warn: (lbs_intersperse) previously exported
  warn: (lbs_iterate) previously exported
  warn: (lbs_last) previously exported
  warn: (lbs_map) previously exported
  warn: (lbs_mapAccumL) previously exported
  warn: (lbs_mapAccumR) previously exported
  warn: (lbs_maximum) previously exported
  warn: (lbs_minimum) previously exported
  warn: (lbs_notElem) previously exported
  warn: (lbs_pack) previously exported
  warn: (lbs_readFile) previously exported
  warn: (lbs_repeat) previously exported
  warn: (lbs_replicate) previously exported
  warn: (lbs_scanl) previously exported
  warn: (lbs_singleton) previously exported
  warn: (lbs_snoc) previously exported
  warn: (lbs_span) previously exported
  warn: (lbs_split) previously exported
  warn: (lbs_splitWith) previously exported
  warn: (lbs_takeWhile) previously exported
  warn: (lbs_uncons) previously exported
  warn: (lbs_unfoldr) previously exported
  warn: (lbs_unpack) previously exported
  warn: (lbs_unsnoc) previously exported
  warn: (lbs_writeFile) previously exported
  warn: (lbs_zip) previously exported
  warn: (lbs_zipWith) previously exported
  warn: (lbs_ByteString) previously exported
  OK (831 elems)
reexporting.. Control.Concurrent.STM...
  OK (901 elems)
reexporting.. Codec.Archive.Tar...
  OK (917 elems)
reexporting.. Codec.Archive.Tar.Entry...
  warn: (tar_entryPath) previously exported
  warn: (tar_Entry) previously exported
  warn: (tar_EntryContent) previously exported
  OK (944 elems)
reexporting.. Codec.Archive.Tar.Check...
  OK (951 elems)
reexporting.. Codec.Compression.GZip...
  OK (979 elems)
reexporting.. Text.Megaparsec...
  OK (1054 elems)
reexporting.. System.Console.ANSI...
  OK (1122 elems)
reexporting.. Options.Applicative...
  info: (nullOption) is not reexported because it is deprecated.
  info: (customExecParserMaybe) is not reexported because it is deprecated.
  info: (execParserMaybe) is not reexported because it is deprecated.
  OK (1221 elems)
reexporting.. Data.Conduit...
  OK (1277 elems)
reexporting.. Data.Conduit.List...
  info: (scanl) is not reexported because it is deprecated.
  info: (scanlM) is not reexported because it is deprecated.
  OK (1318 elems)
reexporting.. Data.Conduit.Binary...
  OK (1341 elems)
reexporting.. Lens.Micro.Platform...
  OK (1407 elems)
reexporting.. ReexportDemo...
  info: (a) is not reexported because it is deprecated.
  OK (1414 elems)
done
```
