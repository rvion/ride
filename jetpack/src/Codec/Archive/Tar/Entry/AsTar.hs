module Codec.Archive.Tar.Entry.AsTar where
-- generated by https://github.com/rvion/ride/tree/master/jetpack-gen

import qualified Codec.Archive.Tar.Entry as I

-- tar_getDirectoryContentsRecursive :: FilePath -> IO [FilePath]
tar_getDirectoryContentsRecursive = I.getDirectoryContentsRecursive

-- tar_packDirectoryEntry :: FilePath -> TarPath -> IO Entry
tar_packDirectoryEntry = I.packDirectoryEntry

-- tar_packFileEntry :: FilePath -> TarPath -> IO Entry
tar_packFileEntry = I.packFileEntry

-- tar_directoryEntry :: TarPath -> Entry
tar_directoryEntry = I.directoryEntry

-- tar_directoryPermissions :: Permissions
tar_directoryPermissions = I.directoryPermissions

-- tar_executableFilePermissions :: Permissions
tar_executableFilePermissions = I.executableFilePermissions

-- tar_fileEntry :: TarPath -> ByteString -> Entry
tar_fileEntry = I.fileEntry

-- tar_fromLinkTarget :: LinkTarget -> FilePath
tar_fromLinkTarget = I.fromLinkTarget

-- tar_fromLinkTargetToPosixPath :: LinkTarget -> FilePath
tar_fromLinkTargetToPosixPath = I.fromLinkTargetToPosixPath

-- tar_fromLinkTargetToWindowsPath :: LinkTarget -> FilePath
tar_fromLinkTargetToWindowsPath = I.fromLinkTargetToWindowsPath

-- tar_fromTarPath :: TarPath -> FilePath
tar_fromTarPath = I.fromTarPath

-- tar_fromTarPathToPosixPath :: TarPath -> FilePath
tar_fromTarPathToPosixPath = I.fromTarPathToPosixPath

-- tar_fromTarPathToWindowsPath :: TarPath -> FilePath
tar_fromTarPathToWindowsPath = I.fromTarPathToWindowsPath

-- tar_ordinaryFilePermissions :: Permissions
tar_ordinaryFilePermissions = I.ordinaryFilePermissions

-- tar_simpleEntry :: TarPath -> EntryContent -> Entry
tar_simpleEntry = I.simpleEntry

-- tar_toLinkTarget :: FilePath -> Maybe LinkTarget
tar_toLinkTarget = I.toLinkTarget

-- tar_toTarPath :: Bool -> FilePath -> Either String TarPath
tar_toTarPath = I.toTarPath

type TarDevMajor  = I.DevMajor
type TarDevMinor  = I.DevMinor
type TarEpochTime  = I.EpochTime
type TarFileSize  = I.FileSize
type TarFormat  = I.Format
tar_mk'V7Format =  I.V7Format-- constructor
pattern TarV7Format  <-  I.V7Format 
tar_mk'UstarFormat =  I.UstarFormat-- constructor
pattern TarUstarFormat  <-  I.UstarFormat 
tar_mk'GnuFormat =  I.GnuFormat-- constructor
pattern TarGnuFormat  <-  I.GnuFormat 
type TarLinkTarget  = I.LinkTarget
type TarOwnership  = I.Ownership
tar_mk'Ownership =  I.Ownership-- constructor
pattern TarOwnership a b c d <-  I.Ownership a b c d
type TarPermissions  = I.Permissions
type TarTarPath  = I.TarPath
type TarTypeCode  = I.TypeCode
