module Data.Conduit.Binary.AsCb where
-- generated by https://github.com/rvion/ride/tree/master/jetpack-gen

import qualified Data.Conduit.Binary as I


-- cb_conduitFile :: forall (m :: * -> *). MonadResource m => FilePath -> Conduit ByteString m ByteString
cb_conduitFile = I.conduitFile

-- cb_conduitHandle :: forall (m :: * -> *). MonadIO m => Handle -> Conduit ByteString m ByteString
cb_conduitHandle = I.conduitHandle

-- cb_drop :: forall (m :: * -> *). Monad m => Int -> Consumer ByteString m ()
cb_drop = I.drop

-- cb_dropWhile :: forall (m :: * -> *). Monad m => (Word8 -> Bool) -> Consumer ByteString m ()
cb_dropWhile = I.dropWhile

-- cb_head :: forall (m :: * -> *). Monad m => Consumer ByteString m (Maybe Word8)
cb_head = I.head

-- cb_isolate :: forall (m :: * -> *). Monad m => Int -> Conduit ByteString m ByteString
cb_isolate = I.isolate

-- cb_lines :: forall (m :: * -> *). Monad m => Conduit ByteString m ByteString
cb_lines = I.lines

-- cb_mapM_ :: forall (m :: * -> *). Monad m => (Word8 -> m ()) -> Consumer ByteString m ()
cb_mapM_ = I.mapM_

-- cb_sinkCacheLength :: forall (m1 :: * -> *) (m2 :: * -> *). (MonadResource m1, MonadResource m2) => Sink ByteString m1 (Word64, Source m2 ByteString)
cb_sinkCacheLength = I.sinkCacheLength

-- cb_sinkFile :: forall (m :: * -> *). MonadResource m => FilePath -> Consumer ByteString m ()
cb_sinkFile = I.sinkFile

-- cb_sinkHandle :: forall (m :: * -> *). MonadIO m => Handle -> Consumer ByteString m ()
cb_sinkHandle = I.sinkHandle

-- cb_sinkIOHandle :: forall (m :: * -> *). MonadResource m => IO Handle -> Consumer ByteString m ()
cb_sinkIOHandle = I.sinkIOHandle

-- cb_sinkLbs :: forall (m :: * -> *). Monad m => Sink ByteString m ByteString
cb_sinkLbs = I.sinkLbs

-- cb_sourceFile :: forall (m :: * -> *). MonadResource m => FilePath -> Producer m ByteString
cb_sourceFile = I.sourceFile

-- cb_sourceFileRange :: forall (m :: * -> *). MonadResource m => FilePath -> Maybe Integer -> Maybe Integer -> Producer m ByteString
cb_sourceFileRange = I.sourceFileRange

-- cb_sourceHandle :: forall (m :: * -> *). MonadIO m => Handle -> Producer m ByteString
cb_sourceHandle = I.sourceHandle

-- cb_sourceHandleRange :: forall (m :: * -> *). MonadIO m => Handle -> Maybe Integer -> Maybe Integer -> Producer m ByteString
cb_sourceHandleRange = I.sourceHandleRange

-- cb_sourceHandleRangeWithBuffer :: forall (m :: * -> *). MonadIO m => Handle -> Maybe Integer -> Maybe Integer -> Int -> Producer m ByteString
cb_sourceHandleRangeWithBuffer = I.sourceHandleRangeWithBuffer

-- cb_sourceHandleUnsafe :: forall (m :: * -> *). MonadIO m => Handle -> Source m ByteString
cb_sourceHandleUnsafe = I.sourceHandleUnsafe

-- cb_sourceIOHandle :: forall (m :: * -> *). MonadResource m => IO Handle -> Producer m ByteString
cb_sourceIOHandle = I.sourceIOHandle

-- cb_sourceLbs :: forall (m :: * -> *). Monad m => ByteString -> Producer m ByteString
cb_sourceLbs = I.sourceLbs

-- cb_take :: forall (m :: * -> *). Monad m => Int -> Consumer ByteString m ByteString
cb_take = I.take

-- cb_takeWhile :: forall (m :: * -> *). Monad m => (Word8 -> Bool) -> Conduit ByteString m ByteString
cb_takeWhile = I.takeWhile
