 -- generated by rvion/jetpack-gen 
module Codec.Archive.Tar.AsTar where
import Codec.Archive.Tar


tar_append :: FilePath -> FilePath -> [FilePath] -> IO ()
tar_append =  T.append
-- (append1) doesn't seem to be exported

tar_create :: FilePath -> FilePath -> [FilePath] -> IO ()
tar_create =  T.create
-- (create1) doesn't seem to be exported

tar_extract :: FilePath -> FilePath -> IO ()
tar_extract =  T.extract
-- (extract1) doesn't seem to be exported
