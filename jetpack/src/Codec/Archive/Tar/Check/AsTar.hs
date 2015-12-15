module Codec.Archive.Tar.Check.AsTar where
-- generated by https://github.com/rvion/ride/tree/master/jetpack-gen

import qualified Codec.Archive.Tar.Check as I

-- tar_checkPortability :: forall e. Entries e -> Entries (Either e PortabilityError)
tar_checkPortability = I.checkPortability

-- tar_checkSecurity :: forall e. Entries e -> Entries (Either e FileNameError)
tar_checkSecurity = I.checkSecurity

-- tar_checkTarbomb :: forall e. FilePath -> Entries e -> Entries (Either e TarBombError)
tar_checkTarbomb = I.checkTarbomb

type TarFileNameError  = I.FileNameError
tar_mk'InvalidFileName =  I.InvalidFileName-- constructor
pattern TarInvalidFileName a <-  I.InvalidFileName a
tar_mk'AbsoluteFileName =  I.AbsoluteFileName-- constructor
pattern TarAbsoluteFileName a <-  I.AbsoluteFileName a
type TarPortabilityError  = I.PortabilityError
tar_mk'NonPortableFormat =  I.NonPortableFormat-- constructor
pattern TarNonPortableFormat a <-  I.NonPortableFormat a
tar_mk'NonPortableFileType =  I.NonPortableFileType-- constructor
pattern TarNonPortableFileType  <-  I.NonPortableFileType 
tar_mk'NonPortableEntryNameChar =  I.NonPortableEntryNameChar-- constructor
pattern TarNonPortableEntryNameChar a <-  I.NonPortableEntryNameChar a
tar_mk'NonPortableFileName =  I.NonPortableFileName-- constructor
pattern TarNonPortableFileName a b <-  I.NonPortableFileName a b
type TarPortabilityPlatform  = I.PortabilityPlatform
type TarTarBombError  = I.TarBombError
tar_mk'TarBombError =  I.TarBombError-- constructor
pattern TarTarBombError a <-  I.TarBombError a
