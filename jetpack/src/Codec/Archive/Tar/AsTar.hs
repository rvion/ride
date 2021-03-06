module Codec.Archive.Tar.AsTar
  ( module Codec.Archive.Tar.AsTar
  ) where
-- generated by https://github.com/rvion/ride/tree/master/jetpack-gen

import qualified Codec.Archive.Tar as I


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

-- constructor :: TruncatedArchive
tar_mk'TruncatedArchive =  I.TruncatedArchive
pattern TarTruncatedArchive  <-  I.TruncatedArchive

-- constructor :: ShortTrailer
tar_mk'ShortTrailer =  I.ShortTrailer
pattern TarShortTrailer  <-  I.ShortTrailer

-- constructor :: BadTrailer
tar_mk'BadTrailer =  I.BadTrailer
pattern TarBadTrailer  <-  I.BadTrailer

-- constructor :: TrailingJunk
tar_mk'TrailingJunk =  I.TrailingJunk
pattern TarTrailingJunk  <-  I.TrailingJunk

-- constructor :: ChecksumIncorrect
tar_mk'ChecksumIncorrect =  I.ChecksumIncorrect
pattern TarChecksumIncorrect  <-  I.ChecksumIncorrect

-- constructor :: NotTarFormat
tar_mk'NotTarFormat =  I.NotTarFormat
pattern TarNotTarFormat  <-  I.NotTarFormat

-- constructor :: UnrecognisedTarFormat
tar_mk'UnrecognisedTarFormat =  I.UnrecognisedTarFormat
pattern TarUnrecognisedTarFormat  <-  I.UnrecognisedTarFormat

-- constructor :: HeaderBadNumericEncoding
tar_mk'HeaderBadNumericEncoding =  I.HeaderBadNumericEncoding
pattern TarHeaderBadNumericEncoding  <-  I.HeaderBadNumericEncoding

type TarEntries a = I.Entries a

-- constructor :: Entry -> Entries e -> Next
tar_mk'Next =  I.Next
pattern TarNext a b <-  I.Next a b

-- constructor :: Done
tar_mk'Done =  I.Done
pattern TarDone  <-  I.Done

-- constructor :: e -> Fail
tar_mk'Fail =  I.Fail
pattern TarFail a <-  I.Fail a

type TarEntry  = I.Entry
get_tar_entryContent o = I.entryContent o
set_tar_entryContent x o = o { I.entryContent = x}

type TarEntryContent  = I.EntryContent

-- constructor :: ByteString -> FileSize -> NormalFile
tar_mk'NormalFile =  I.NormalFile
pattern TarNormalFile a b <-  I.NormalFile a b

-- constructor :: Directory
tar_mk'Directory =  I.Directory
pattern TarDirectory  <-  I.Directory

-- constructor :: LinkTarget -> SymbolicLink
tar_mk'SymbolicLink =  I.SymbolicLink
pattern TarSymbolicLink a <-  I.SymbolicLink a

-- constructor :: LinkTarget -> HardLink
tar_mk'HardLink =  I.HardLink
pattern TarHardLink a <-  I.HardLink a

-- constructor :: DevMajor -> DevMinor -> CharacterDevice
tar_mk'CharacterDevice =  I.CharacterDevice
pattern TarCharacterDevice a b <-  I.CharacterDevice a b

-- constructor :: DevMajor -> DevMinor -> BlockDevice
tar_mk'BlockDevice =  I.BlockDevice
pattern TarBlockDevice a b <-  I.BlockDevice a b

-- constructor :: NamedPipe
tar_mk'NamedPipe =  I.NamedPipe
pattern TarNamedPipe  <-  I.NamedPipe

-- constructor :: TypeCode -> ByteString -> FileSize -> OtherEntryType
tar_mk'OtherEntryType =  I.OtherEntryType
pattern TarOtherEntryType a b c <-  I.OtherEntryType a b c
