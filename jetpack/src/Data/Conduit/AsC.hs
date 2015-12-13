module Data.Conduit.AsC where
-- generated by https://github.com/rvion/ride/tree/master/jetpack-gen

import Data.Conduit as I

-- c_connect :: forall (m :: * -> *) a b. Monad m => Source m a -> Sink a m b -> m b
c_connect = I.connect

-- c_fuse :: forall a (m :: * -> *) b c r. Monad m => Conduit a m b -> ConduitM b c m r -> ConduitM a c m r
c_fuse = I.fuse

-- ($$) :: forall (m :: * -> *) a b. Monad m => Source m a -> Sink a m b -> m b
($$) = (I.$$)

-- ($$+) :: forall (m :: * -> *) a b. Monad m => Source m a -> Sink a m b -> m (ResumableSource m a, b)
($$+) = (I.$$+)

-- ($$++) :: forall (m :: * -> *) a b. Monad m => ResumableSource m a -> Sink a m b -> m (ResumableSource m a, b)
($$++) = (I.$$++)

-- ($$+-) :: forall (m :: * -> *) a b. Monad m => ResumableSource m a -> Sink a m b -> m b
($$+-) = (I.$$+-)

-- ($=) :: forall a (m :: * -> *) b c r. Monad m => Conduit a m b -> ConduitM b c m r -> ConduitM a c m r
($=) = (I.$=)

-- ($=+) :: forall (m :: * -> *) a b. Monad m => ResumableSource m a -> Conduit a m b -> ResumableSource m b
($=+) = (I.$=+)

-- (=$) :: forall a (m :: * -> *) b c r. Monad m => Conduit a m b -> ConduitM b c m r -> ConduitM a c m r
(=$) = (I.=$)

-- (=$$+) :: forall a (m :: * -> *) b r. Monad m => Conduit a m b -> Sink b m r -> Sink a m (ResumableConduit a m b, r)
(=$$+) = (I.=$$+)

-- (=$$++) :: forall i (m :: * -> *) o r. Monad m => ResumableConduit i m o -> Sink o m r -> Sink i m (ResumableConduit i m o, r)
(=$$++) = (I.=$$++)

-- (=$$+-) :: forall i (m :: * -> *) o r. Monad m => ResumableConduit i m o -> Sink o m r -> Sink i m r
(=$$+-) = (I.=$$+-)

-- (=$=) :: forall a (m :: * -> *) b c r. Monad m => Conduit a m b -> ConduitM b c m r -> ConduitM a c m r
(=$=) = (I.=$=)

-- c_addCleanup :: forall (m :: * -> *) i o r. Monad m => (Bool -> m ()) -> ConduitM i o m r -> ConduitM i o m r
c_addCleanup = I.addCleanup

-- c_await :: forall i (m :: * -> *). Monad m => Consumer i m (Maybe i)
c_await = I.await

-- c_awaitForever :: forall i o (m :: * -> *) r. Monad m => (i -> ConduitM i o m r) -> ConduitM i o m ()
c_awaitForever = I.awaitForever

-- c_bracketP :: forall a i o (m :: * -> *) r. MonadResource m => IO a -> (a -> IO ()) -> (a -> ConduitM i o m r) -> ConduitM i o m r
c_bracketP = I.bracketP

-- c_catchC :: forall i o (m :: * -> *) r e. (MonadBaseControl IO m, Exception e) => ConduitM i o m r -> (e -> ConduitM i o m r) -> ConduitM i o m r
c_catchC = I.catchC

-- c_closeResumableSource :: forall (m :: * -> *) a. Monad m => ResumableSource m a -> m ()
c_closeResumableSource = I.closeResumableSource

-- c_fuseBoth :: forall a b (m :: * -> *) r1 c r2. Monad m => ConduitM a b m r1 -> ConduitM b c m r2 -> ConduitM a c m (r1, r2)
c_fuseBoth = I.fuseBoth

-- c_fuseBothMaybe :: forall a b (m :: * -> *) r1 c r2. Monad m => ConduitM a b m r1 -> ConduitM b c m r2 -> ConduitM a c m (Maybe r1, r2)
c_fuseBothMaybe = I.fuseBothMaybe

-- c_fuseLeftovers :: forall b a (m :: * -> *) c r. Monad m => ([b] -> [a]) -> ConduitM a b m () -> ConduitM b c m r -> ConduitM a c m r
c_fuseLeftovers = I.fuseLeftovers

-- c_fuseReturnLeftovers :: forall a b (m :: * -> *) c r. Monad m => ConduitM a b m () -> ConduitM b c m r -> ConduitM a c m (r, [b])
c_fuseReturnLeftovers = I.fuseReturnLeftovers

-- c_fuseUpstream :: forall a b (m :: * -> *) r c. Monad m => ConduitM a b m r -> Conduit b m c -> ConduitM a c m r
c_fuseUpstream = I.fuseUpstream

-- c_handleC :: forall e i o (m :: * -> *) r. (MonadBaseControl IO m, Exception e) => (e -> ConduitM i o m r) -> ConduitM i o m r -> ConduitM i o m r
c_handleC = I.handleC

-- c_leftover :: forall i o (m :: * -> *). i -> ConduitM i o m ()
c_leftover = I.leftover

-- c_mapInput :: forall i1 i2 o (m :: * -> *) r. Monad m => (i1 -> i2) -> (i2 -> Maybe i1) -> ConduitM i2 o m r -> ConduitM i1 o m r
c_mapInput = I.mapInput

-- c_mapOutput :: forall o1 o2 i (m :: * -> *) r. Monad m => (o1 -> o2) -> ConduitM i o1 m r -> ConduitM i o2 m r
c_mapOutput = I.mapOutput

-- c_mapOutputMaybe :: forall o1 o2 i (m :: * -> *) r. Monad m => (o1 -> Maybe o2) -> ConduitM i o1 m r -> ConduitM i o2 m r
c_mapOutputMaybe = I.mapOutputMaybe

-- c_newResumableConduit :: forall i (m :: * -> *) o. Monad m => Conduit i m o -> ResumableConduit i m o
c_newResumableConduit = I.newResumableConduit

-- c_newResumableSource :: forall (m :: * -> *) o. Monad m => Source m o -> ResumableSource m o
c_newResumableSource = I.newResumableSource

-- c_passthroughSink :: forall i (m :: * -> *) r. Monad m => Sink i m r -> (r -> m ()) -> Conduit i m i
c_passthroughSink = I.passthroughSink

-- c_runConduit :: forall (m :: * -> *) r. Monad m => ConduitM () Void m r -> m r
c_runConduit = I.runConduit

-- c_sequenceConduits :: forall (f :: * -> *) i o (m :: * -> *) r. (Traversable f, Monad m) => f (ConduitM i o m r) -> ConduitM i o m (f r)
c_sequenceConduits = I.sequenceConduits

-- c_sequenceSinks :: forall (f :: * -> *) i (m :: * -> *) r. (Traversable f, Monad m) => f (Sink i m r) -> Sink i m (f r)
c_sequenceSinks = I.sequenceSinks

-- c_sequenceSources :: forall (f :: * -> *) (m :: * -> *) o. (Traversable f, Monad m) => f (Source m o) -> Source m (f o)
c_sequenceSources = I.sequenceSources

-- c_toConsumer :: forall a (m :: * -> *) b. Monad m => Sink a m b -> Consumer a m b
c_toConsumer = I.toConsumer

-- c_toProducer :: forall (m :: * -> *) a. Monad m => Source m a -> Producer m a
c_toProducer = I.toProducer

-- c_transPipe :: forall i o (m :: * -> *) r (n :: * -> *). Monad m => (forall a. m a -> n a) -> ConduitM i o m r -> ConduitM i o n r
c_transPipe = I.transPipe

-- c_tryC :: forall i o (m :: * -> *) r e. (MonadBaseControl IO m, Exception e) => ConduitM i o m r -> ConduitM i o m (Either e r)
c_tryC = I.tryC

-- c_unwrapResumable :: forall (m :: * -> *) o. MonadIO m => ResumableSource m o -> m (Source m o, m ())
c_unwrapResumable = I.unwrapResumable

-- c_unwrapResumableConduit :: forall i (m :: * -> *) o. MonadIO m => ResumableConduit i m o -> m (Conduit i m o, m ())
c_unwrapResumableConduit = I.unwrapResumableConduit

-- c_yield :: forall o i (m :: * -> *). Monad m => o -> ConduitM i o m ()
c_yield = I.yield

-- c_yieldOr :: forall o (m :: * -> *) i. Monad m => o -> m () -> ConduitM i o m ()
c_yieldOr = I.yieldOr

type CConduit a b c = I.Conduit a b c
type CConduitM a b c d = I.ConduitM a b c d
type CConsumer a b c = I.Consumer a b c
type CFlush a = I.Flush a
type CProducer a b = I.Producer a b
type CResumableConduit a b c = I.ResumableConduit a b c
type CResumableSource a b = I.ResumableSource a b
type CSink a = I.Sink a
type CSource a b = I.Source a b
type CZipConduit a b c d = I.ZipConduit a b c d
type CZipSink a b c = I.ZipSink a b c
type CZipSource a b = I.ZipSource a b
