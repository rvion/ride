module Codec.Archive.Tar.AsTar where
-- generated by rvion/jetpack-gen 

import Codec.Archive.Tar as I

tar_append = I.append
tar_create = I.create
tar_extract = I.extract
tar_pack = I.pack
tar_read = I.read
tar_entryPath = I.entryPath
tar_foldEntries = I.foldEntries
tar_mapEntries = I.mapEntries
tar_mapEntriesNoFail = I.mapEntriesNoFail
tar_unfoldEntries = I.unfoldEntries
tar_unpack = I.unpack
tar_write = I.write
type TarFormatError  = I.FormatError 
type TarEntries a = I.Entries a
type TarEntry  = I.Entry 
type TarEntryContent  = I.EntryContent 
