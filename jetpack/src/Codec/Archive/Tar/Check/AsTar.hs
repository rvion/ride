{-# LANGUAGE NoMonomorphismRestriction #-}

module Codec.Archive.Tar.Check.AsTar where
-- generated by rvion/jetpack-gen 

import Codec.Archive.Tar.Check as I

type Tar_FileNameError = I.FileNameError
type Tar_PortabilityError = I.PortabilityError
-- (PortabilityPlatform) :: IfaceSynonym -> NOT YET SUPPORTED
type Tar_TarBombError = I.TarBombError
tar_checkPortability =  I.checkPortability
tar_checkSecurity =  I.checkSecurity
tar_checkTarbomb =  I.checkTarbomb
