module Codec.Archive.Tar.AsTar where
-- generated by https://github.com/rvion/ride/tree/master/jetpack-gen

import           Codec.Archive.Tar as I

-- tar_append :: FilePath -> FilePath -> [FilePath] -> IO ()
tar_append = I.append

-- tar_create :: FilePath -> FilePath -> [FilePath] -> IO ()
tar_create = I.create

-- tar_extract :: FilePath -> FilePath -> IO ()
tar_extract = I.extract

-- tar_pack :: FilePath -> [FilePath] -> IO [Entry]
tar_pack = I.pack

-- tar_read :: ByteString -> Entries FormatError
tar_read = I.read

-- tar_entryPath :: Entry -> FilePath
tar_entryPath = I.entryPath

-- tar_foldEntries :: forall a e. (Entry -> a -> a) -> a -> (e -> a) -> Entries e -> a
tar_foldEntries = I.foldEntries

-- tar_mapEntries :: forall e' e. (Entry -> Either e' Entry) -> Entries e -> Entries (Either e e')
tar_mapEntries = I.mapEntries

-- tar_mapEntriesNoFail :: forall e. (Entry -> Entry) -> Entries e -> Entries e
tar_mapEntriesNoFail = I.mapEntriesNoFail

-- tar_unfoldEntries :: forall a e. (a -> Either e (Maybe (Entry, a))) -> a -> Entries e
tar_unfoldEntries = I.unfoldEntries

-- tar_unpack :: forall e. Exception e => FilePath -> Entries e -> IO ()
tar_unpack = I.unpack

-- tar_write :: [Entry] -> ByteString
tar_write = I.write

type TarFormatError  = I.FormatError
type TarEntries a = I.Entries a
type TarEntry  = I.Entry
type TarEntryContent  = I.EntryContent
