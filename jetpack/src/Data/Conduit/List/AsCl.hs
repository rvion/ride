module Data.Conduit.List.AsCl where
-- generated by rvion/jetpack-gen 

import Data.Conduit.List as I

-- cl_catMaybes :: forall a (m :: * -> *). Monad m => Conduit (Maybe a) m a
cl_catMaybes = I.catMaybes
-- cl_concat :: forall (f :: * -> *) a (m :: * -> *).
(Monad m, Foldable f) =>
Conduit (f a) m a
cl_concat = I.concat
-- cl_concatMap :: forall a b (m :: * -> *). Monad m => (a -> [b]) -> Conduit a m b
cl_concatMap = I.concatMap
-- cl_concatMapAccum :: forall a accum b (m :: * -> *).
Monad m =>
(a -> accum -> (accum, [b])) -> accum -> Conduit a m b
cl_concatMapAccum = I.concatMapAccum
-- cl_concatMapAccumM :: forall a accum (m :: * -> *) b.
Monad m =>
(a -> accum -> m (accum, [b])) -> accum -> Conduit a m b
cl_concatMapAccumM = I.concatMapAccumM
-- cl_concatMapM :: forall a (m :: * -> *) b. Monad m => (a -> m [b]) -> Conduit a m b
cl_concatMapM = I.concatMapM
-- cl_consume :: forall a (m :: * -> *). Monad m => Consumer a m [a]
cl_consume = I.consume
-- cl_drop :: forall a (m :: * -> *). Monad m => Int -> Consumer a m ()
cl_drop = I.drop
-- cl_enumFromTo :: forall a (m :: * -> *).
(Enum a, Ord a, Monad m) =>
a -> a -> Producer m a
cl_enumFromTo = I.enumFromTo
-- cl_filter :: forall a (m :: * -> *). Monad m => (a -> Bool) -> Conduit a m a
cl_filter = I.filter
-- cl_fold :: forall b a (m :: * -> *).
Monad m =>
(b -> a -> b) -> b -> Consumer a m b
cl_fold = I.fold
-- cl_foldM :: forall b a (m :: * -> *).
Monad m =>
(b -> a -> m b) -> b -> Consumer a m b
cl_foldM = I.foldM
-- cl_foldMap :: forall a b (m :: * -> *).
(Monad m, Monoid b) =>
(a -> b) -> Consumer a m b
cl_foldMap = I.foldMap
-- cl_foldMapM :: forall a (m :: * -> *) b.
(Monad m, Monoid b) =>
(a -> m b) -> Consumer a m b
cl_foldMapM = I.foldMapM
-- cl_groupBy :: forall a (m :: * -> *).
Monad m =>
(a -> a -> Bool) -> Conduit a m [a]
cl_groupBy = I.groupBy
-- cl_groupOn1 :: forall a b (m :: * -> *).
(Monad m, Eq b) =>
(a -> b) -> Conduit a m (a, [a])
cl_groupOn1 = I.groupOn1
-- cl_head :: forall a (m :: * -> *). Monad m => Consumer a m (Maybe a)
cl_head = I.head
-- cl_isolate :: forall a (m :: * -> *). Monad m => Int -> Conduit a m a
cl_isolate = I.isolate
-- cl_iterM :: forall a (m :: * -> *). Monad m => (a -> m ()) -> Conduit a m a
cl_iterM = I.iterM
-- cl_iterate :: forall a (m :: * -> *). Monad m => (a -> a) -> a -> Producer m a
cl_iterate = I.iterate
-- cl_map :: forall a b (m :: * -> *). Monad m => (a -> b) -> Conduit a m b
cl_map = I.map
-- cl_mapAccum :: forall a s b (m :: * -> *).
Monad m =>
(a -> s -> (s, b)) -> s -> ConduitM a b m s
cl_mapAccum = I.mapAccum
-- cl_mapAccumM :: forall a s (m :: * -> *) b.
Monad m =>
(a -> s -> m (s, b)) -> s -> ConduitM a b m s
cl_mapAccumM = I.mapAccumM
-- cl_mapFoldable :: forall a (f :: * -> *) b (m :: * -> *).
(Monad m, Foldable f) =>
(a -> f b) -> Conduit a m b
cl_mapFoldable = I.mapFoldable
-- cl_mapFoldableM :: forall a (m :: * -> *) (f :: * -> *) b.
(Monad m, Foldable f) =>
(a -> m (f b)) -> Conduit a m b
cl_mapFoldableM = I.mapFoldableM
-- cl_mapM :: forall a (m :: * -> *) b. Monad m => (a -> m b) -> Conduit a m b
cl_mapM = I.mapM
-- cl_mapM_ :: forall a (m :: * -> *). Monad m => (a -> m ()) -> Consumer a m ()
cl_mapM_ = I.mapM_
-- cl_mapMaybe :: forall a b (m :: * -> *).
Monad m =>
(a -> Maybe b) -> Conduit a m b
cl_mapMaybe = I.mapMaybe
-- cl_mapMaybeM :: forall a (m :: * -> *) b.
Monad m =>
(a -> m (Maybe b)) -> Conduit a m b
cl_mapMaybeM = I.mapMaybeM
-- cl_peek :: forall a (m :: * -> *). Monad m => Consumer a m (Maybe a)
cl_peek = I.peek
-- cl_replicate :: forall a (m :: * -> *). Monad m => Int -> a -> Producer m a
cl_replicate = I.replicate
-- cl_replicateM :: forall (m :: * -> *) a. Monad m => Int -> m a -> Producer m a
cl_replicateM = I.replicateM
-- cl_scan :: forall a b (m :: * -> *).
Monad m =>
(a -> b -> b) -> b -> ConduitM a b m b
cl_scan = I.scan
-- cl_scanM :: forall a b (m :: * -> *).
Monad m =>
(a -> b -> m b) -> b -> ConduitM a b m b
cl_scanM = I.scanM
-- cl_sequence :: forall i (m :: * -> *) o.
Monad m =>
Consumer i m o -> Conduit i m o
cl_sequence = I.sequence
-- cl_sinkNull :: forall a (m :: * -> *). Monad m => Consumer a m ()
cl_sinkNull = I.sinkNull
-- cl_sourceList :: forall a (m :: * -> *). Monad m => [a] -> Producer m a
cl_sourceList = I.sourceList
-- cl_sourceNull :: forall (m :: * -> *) a. Monad m => Producer m a
cl_sourceNull = I.sourceNull
-- cl_take :: forall a (m :: * -> *). Monad m => Int -> Consumer a m [a]
cl_take = I.take
-- cl_unfold :: forall b a (m :: * -> *).
Monad m =>
(b -> Maybe (a, b)) -> b -> Producer m a
cl_unfold = I.unfold
-- cl_unfoldM :: forall b (m :: * -> *) a.
Monad m =>
(b -> m (Maybe (a, b))) -> b -> Producer m a
cl_unfoldM = I.unfoldM
