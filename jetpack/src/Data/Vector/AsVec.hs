module Data.Vector.AsVec
  ( module Data.Vector.AsVec
  ) where
-- generated by https://github.com/rvion/ride/tree/master/jetpack-gen

import qualified Data.Vector as I


-- (!?) :: forall a. Vector a -> Int -> Maybe a
(!?) = (I.!?)

-- (++) :: forall a. Vector a -> Vector a -> Vector a
(++) = (I.++)

-- (//) :: forall a. Vector a -> [(Int, a)] -> Vector a
(//) = (I.//)

-- vec_accum :: forall a b. (a -> b -> a) -> Vector a -> [(Int, b)] -> Vector a
vec_accum = I.accum

-- vec_accumulate :: forall a b. (a -> b -> a) -> Vector a -> Vector (Int, b) -> Vector a
vec_accumulate = I.accumulate

-- vec_accumulate_ :: forall a b. (a -> b -> a) -> Vector a -> Vector Int -> Vector b -> Vector a
vec_accumulate_ = I.accumulate_

-- vec_all :: forall a. (a -> Bool) -> Vector a -> Bool
vec_all = I.all

-- vec_and :: Vector Bool -> Bool
vec_and = I.and

-- vec_any :: forall a. (a -> Bool) -> Vector a -> Bool
vec_any = I.any

-- vec_backpermute :: forall a. Vector a -> Vector Int -> Vector a
vec_backpermute = I.backpermute

-- vec_break :: forall a. (a -> Bool) -> Vector a -> (Vector a, Vector a)
vec_break = I.break

-- vec_concat :: forall a. [Vector a] -> Vector a
vec_concat = I.concat

-- vec_concatMap :: forall a b. (a -> Vector b) -> Vector a -> Vector b
vec_concatMap = I.concatMap

-- vec_cons :: forall a. a -> Vector a -> Vector a
vec_cons = I.cons

-- vec_constructN :: forall a. Int -> (Vector a -> a) -> Vector a
vec_constructN = I.constructN

-- vec_constructrN :: forall a. Int -> (Vector a -> a) -> Vector a
vec_constructrN = I.constructrN

-- vec_copy :: forall (m :: * -> *) a. PrimMonad m => MVector (PrimState m) a -> Vector a -> m ()
vec_copy = I.copy

-- vec_create :: forall a. (forall s. ST s (MVector s a)) -> Vector a
vec_create = I.create

-- vec_drop :: forall a. Int -> Vector a -> Vector a
vec_drop = I.drop

-- vec_dropWhile :: forall a. (a -> Bool) -> Vector a -> Vector a
vec_dropWhile = I.dropWhile

-- vec_elem :: forall a. Eq a => a -> Vector a -> Bool
vec_elem = I.elem

-- vec_elemIndex :: forall a. Eq a => a -> Vector a -> Maybe Int
vec_elemIndex = I.elemIndex

-- vec_elemIndices :: forall a. Eq a => a -> Vector a -> Vector Int
vec_elemIndices = I.elemIndices

-- vec_empty :: forall a. Vector a
vec_empty = I.empty

-- vec_enumFromN :: forall a. Num a => a -> Int -> Vector a
vec_enumFromN = I.enumFromN

-- vec_enumFromStepN :: forall a. Num a => a -> a -> Int -> Vector a
vec_enumFromStepN = I.enumFromStepN

-- vec_enumFromThenTo :: forall a. Enum a => a -> a -> a -> Vector a
vec_enumFromThenTo = I.enumFromThenTo

-- vec_enumFromTo :: forall a. Enum a => a -> a -> Vector a
vec_enumFromTo = I.enumFromTo

-- vec_filter :: forall a. (a -> Bool) -> Vector a -> Vector a
vec_filter = I.filter

-- vec_filterM :: forall a (m :: * -> *). Monad m => (a -> m Bool) -> Vector a -> m (Vector a)
vec_filterM = I.filterM

-- vec_find :: forall a. (a -> Bool) -> Vector a -> Maybe a
vec_find = I.find

-- vec_findIndex :: forall a. (a -> Bool) -> Vector a -> Maybe Int
vec_findIndex = I.findIndex

-- vec_findIndices :: forall a. (a -> Bool) -> Vector a -> Vector Int
vec_findIndices = I.findIndices

-- vec_fold1M :: forall a (m :: * -> *). Monad m => (a -> a -> m a) -> Vector a -> m a
vec_fold1M = I.fold1M

-- vec_fold1M' :: forall a (m :: * -> *). Monad m => (a -> a -> m a) -> Vector a -> m a
vec_fold1M' = I.fold1M'

-- vec_fold1M'_ :: forall a (m :: * -> *). Monad m => (a -> a -> m a) -> Vector a -> m ()
vec_fold1M'_ = I.fold1M'_

-- vec_fold1M_ :: forall a (m :: * -> *). Monad m => (a -> a -> m a) -> Vector a -> m ()
vec_fold1M_ = I.fold1M_

-- vec_foldM :: forall a b (m :: * -> *). Monad m => (a -> b -> m a) -> a -> Vector b -> m a
vec_foldM = I.foldM

-- vec_foldM' :: forall a b (m :: * -> *). Monad m => (a -> b -> m a) -> a -> Vector b -> m a
vec_foldM' = I.foldM'

-- vec_foldM'_ :: forall a b (m :: * -> *). Monad m => (a -> b -> m a) -> a -> Vector b -> m ()
vec_foldM'_ = I.foldM'_

-- vec_foldM_ :: forall a b (m :: * -> *). Monad m => (a -> b -> m a) -> a -> Vector b -> m ()
vec_foldM_ = I.foldM_

-- vec_foldl :: forall a b. (a -> b -> a) -> a -> Vector b -> a
vec_foldl = I.foldl

-- vec_foldl' :: forall a b. (a -> b -> a) -> a -> Vector b -> a
vec_foldl' = I.foldl'

-- vec_foldl1 :: forall a. (a -> a -> a) -> Vector a -> a
vec_foldl1 = I.foldl1

-- vec_foldl1' :: forall a. (a -> a -> a) -> Vector a -> a
vec_foldl1' = I.foldl1'

-- vec_foldr :: forall a b. (a -> b -> b) -> b -> Vector a -> b
vec_foldr = I.foldr

-- vec_foldr' :: forall a b. (a -> b -> b) -> b -> Vector a -> b
vec_foldr' = I.foldr'

-- vec_foldr1 :: forall a. (a -> a -> a) -> Vector a -> a
vec_foldr1 = I.foldr1

-- vec_foldr1' :: forall a. (a -> a -> a) -> Vector a -> a
vec_foldr1' = I.foldr1'

-- vec_forM :: forall a (m :: * -> *) b. Monad m => Vector a -> (a -> m b) -> m (Vector b)
vec_forM = I.forM

-- vec_forM_ :: forall a (m :: * -> *) b. Monad m => Vector a -> (a -> m b) -> m ()
vec_forM_ = I.forM_

-- vec_force :: forall a. Vector a -> Vector a
vec_force = I.force

-- vec_freeze :: forall (m :: * -> *) a. PrimMonad m => MVector (PrimState m) a -> m (Vector a)
vec_freeze = I.freeze

-- vec_fromList :: forall a. [a] -> Vector a
vec_fromList = I.fromList

-- vec_fromListN :: forall a. Int -> [a] -> Vector a
vec_fromListN = I.fromListN

-- vec_generate :: forall a. Int -> (Int -> a) -> Vector a
vec_generate = I.generate

-- vec_generateM :: forall (m :: * -> *) a. Monad m => Int -> (Int -> m a) -> m (Vector a)
vec_generateM = I.generateM

-- vec_head :: forall a. Vector a -> a
vec_head = I.head

-- vec_headM :: forall a (m :: * -> *). Monad m => Vector a -> m a
vec_headM = I.headM

-- vec_ifilter :: forall a. (Int -> a -> Bool) -> Vector a -> Vector a
vec_ifilter = I.ifilter

-- vec_ifoldM :: forall a b (m :: * -> *). Monad m => (a -> Int -> b -> m a) -> a -> Vector b -> m a
vec_ifoldM = I.ifoldM

-- vec_ifoldM' :: forall a b (m :: * -> *). Monad m => (a -> Int -> b -> m a) -> a -> Vector b -> m a
vec_ifoldM' = I.ifoldM'

-- vec_ifoldM'_ :: forall a b (m :: * -> *). Monad m => (a -> Int -> b -> m a) -> a -> Vector b -> m ()
vec_ifoldM'_ = I.ifoldM'_

-- vec_ifoldM_ :: forall a b (m :: * -> *). Monad m => (a -> Int -> b -> m a) -> a -> Vector b -> m ()
vec_ifoldM_ = I.ifoldM_

-- vec_ifoldl :: forall a b. (a -> Int -> b -> a) -> a -> Vector b -> a
vec_ifoldl = I.ifoldl

-- vec_ifoldl' :: forall a b. (a -> Int -> b -> a) -> a -> Vector b -> a
vec_ifoldl' = I.ifoldl'

-- vec_ifoldr :: forall a b. (Int -> a -> b -> b) -> b -> Vector a -> b
vec_ifoldr = I.ifoldr

-- vec_ifoldr' :: forall a b. (Int -> a -> b -> b) -> b -> Vector a -> b
vec_ifoldr' = I.ifoldr'

-- vec_imap :: forall a b. (Int -> a -> b) -> Vector a -> Vector b
vec_imap = I.imap

-- vec_imapM :: forall a (m :: * -> *) b. Monad m => (Int -> a -> m b) -> Vector a -> m (Vector b)
vec_imapM = I.imapM

-- vec_imapM_ :: forall a (m :: * -> *) b. Monad m => (Int -> a -> m b) -> Vector a -> m ()
vec_imapM_ = I.imapM_

-- vec_indexM :: forall a (m :: * -> *). Monad m => Vector a -> Int -> m a
vec_indexM = I.indexM

-- vec_indexed :: forall a. Vector a -> Vector (Int, a)
vec_indexed = I.indexed

-- vec_init :: forall a. Vector a -> Vector a
vec_init = I.init

-- vec_iterateN :: forall a. Int -> (a -> a) -> a -> Vector a
vec_iterateN = I.iterateN

-- vec_izipWith :: forall a b c. (Int -> a -> b -> c) -> Vector a -> Vector b -> Vector c
vec_izipWith = I.izipWith

-- vec_izipWith3 :: forall a b c d. (Int -> a -> b -> c -> d) -> Vector a -> Vector b -> Vector c -> Vector d
vec_izipWith3 = I.izipWith3

-- vec_izipWith4 :: forall a b c d e. (Int -> a -> b -> c -> d -> e) -> Vector a -> Vector b -> Vector c -> Vector d -> Vector e
vec_izipWith4 = I.izipWith4

-- vec_izipWith5 :: forall a b c d e f. (Int -> a -> b -> c -> d -> e -> f) -> Vector a -> Vector b -> Vector c -> Vector d -> Vector e -> Vector f
vec_izipWith5 = I.izipWith5

-- vec_izipWith6 :: forall a b c d e f g. (Int -> a -> b -> c -> d -> e -> f -> g) -> Vector a -> Vector b -> Vector c -> Vector d -> Vector e -> Vector f -> Vector g
vec_izipWith6 = I.izipWith6

-- vec_izipWithM :: forall a b (m :: * -> *) c. Monad m => (Int -> a -> b -> m c) -> Vector a -> Vector b -> m (Vector c)
vec_izipWithM = I.izipWithM

-- vec_izipWithM_ :: forall a b (m :: * -> *) c. Monad m => (Int -> a -> b -> m c) -> Vector a -> Vector b -> m ()
vec_izipWithM_ = I.izipWithM_

-- vec_last :: forall a. Vector a -> a
vec_last = I.last

-- vec_lastM :: forall a (m :: * -> *). Monad m => Vector a -> m a
vec_lastM = I.lastM

-- vec_length :: forall a. Vector a -> Int
vec_length = I.length

-- vec_map :: forall a b. (a -> b) -> Vector a -> Vector b
vec_map = I.map

-- vec_mapM :: forall a (m :: * -> *) b. Monad m => (a -> m b) -> Vector a -> m (Vector b)
vec_mapM = I.mapM

-- vec_mapM_ :: forall a (m :: * -> *) b. Monad m => (a -> m b) -> Vector a -> m ()
vec_mapM_ = I.mapM_

-- vec_maxIndex :: forall a. Ord a => Vector a -> Int
vec_maxIndex = I.maxIndex

-- vec_maxIndexBy :: forall a. (a -> a -> Ordering) -> Vector a -> Int
vec_maxIndexBy = I.maxIndexBy

-- vec_maximum :: forall a. Ord a => Vector a -> a
vec_maximum = I.maximum

-- vec_maximumBy :: forall a. (a -> a -> Ordering) -> Vector a -> a
vec_maximumBy = I.maximumBy

-- vec_minIndex :: forall a. Ord a => Vector a -> Int
vec_minIndex = I.minIndex

-- vec_minIndexBy :: forall a. (a -> a -> Ordering) -> Vector a -> Int
vec_minIndexBy = I.minIndexBy

-- vec_minimum :: forall a. Ord a => Vector a -> a
vec_minimum = I.minimum

-- vec_minimumBy :: forall a. (a -> a -> Ordering) -> Vector a -> a
vec_minimumBy = I.minimumBy

-- vec_modify :: forall a. (forall s. MVector s a -> ST s ()) -> Vector a -> Vector a
vec_modify = I.modify

-- vec_notElem :: forall a. Eq a => a -> Vector a -> Bool
vec_notElem = I.notElem

-- vec_null :: forall a. Vector a -> Bool
vec_null = I.null

-- vec_or :: Vector Bool -> Bool
vec_or = I.or

-- vec_partition :: forall a. (a -> Bool) -> Vector a -> (Vector a, Vector a)
vec_partition = I.partition

-- vec_postscanl :: forall a b. (a -> b -> a) -> a -> Vector b -> Vector a
vec_postscanl = I.postscanl

-- vec_postscanl' :: forall a b. (a -> b -> a) -> a -> Vector b -> Vector a
vec_postscanl' = I.postscanl'

-- vec_postscanr :: forall a b. (a -> b -> b) -> b -> Vector a -> Vector b
vec_postscanr = I.postscanr

-- vec_postscanr' :: forall a b. (a -> b -> b) -> b -> Vector a -> Vector b
vec_postscanr' = I.postscanr'

-- vec_prescanl :: forall a b. (a -> b -> a) -> a -> Vector b -> Vector a
vec_prescanl = I.prescanl

-- vec_prescanl' :: forall a b. (a -> b -> a) -> a -> Vector b -> Vector a
vec_prescanl' = I.prescanl'

-- vec_prescanr :: forall a b. (a -> b -> b) -> b -> Vector a -> Vector b
vec_prescanr = I.prescanr

-- vec_prescanr' :: forall a b. (a -> b -> b) -> b -> Vector a -> Vector b
vec_prescanr' = I.prescanr'

-- vec_product :: forall a. Num a => Vector a -> a
vec_product = I.product

-- vec_replicate :: forall a. Int -> a -> Vector a
vec_replicate = I.replicate

-- vec_replicateM :: forall (m :: * -> *) a. Monad m => Int -> m a -> m (Vector a)
vec_replicateM = I.replicateM

-- vec_reverse :: forall a. Vector a -> Vector a
vec_reverse = I.reverse

-- vec_scanl :: forall a b. (a -> b -> a) -> a -> Vector b -> Vector a
vec_scanl = I.scanl

-- vec_scanl' :: forall a b. (a -> b -> a) -> a -> Vector b -> Vector a
vec_scanl' = I.scanl'

-- vec_scanl1 :: forall a. (a -> a -> a) -> Vector a -> Vector a
vec_scanl1 = I.scanl1

-- vec_scanl1' :: forall a. (a -> a -> a) -> Vector a -> Vector a
vec_scanl1' = I.scanl1'

-- vec_scanr :: forall a b. (a -> b -> b) -> b -> Vector a -> Vector b
vec_scanr = I.scanr

-- vec_scanr' :: forall a b. (a -> b -> b) -> b -> Vector a -> Vector b
vec_scanr' = I.scanr'

-- vec_scanr1 :: forall a. (a -> a -> a) -> Vector a -> Vector a
vec_scanr1 = I.scanr1

-- vec_scanr1' :: forall a. (a -> a -> a) -> Vector a -> Vector a
vec_scanr1' = I.scanr1'

-- vec_sequence :: forall (m :: * -> *) a. Monad m => Vector (m a) -> m (Vector a)
vec_sequence = I.sequence

-- vec_sequence_ :: forall (m :: * -> *) a. Monad m => Vector (m a) -> m ()
vec_sequence_ = I.sequence_

-- vec_singleton :: forall a. a -> Vector a
vec_singleton = I.singleton

-- vec_slice :: forall a. Int -> Int -> Vector a -> Vector a
vec_slice = I.slice

-- vec_snoc :: forall a. Vector a -> a -> Vector a
vec_snoc = I.snoc

-- vec_span :: forall a. (a -> Bool) -> Vector a -> (Vector a, Vector a)
vec_span = I.span

-- vec_splitAt :: forall a. Int -> Vector a -> (Vector a, Vector a)
vec_splitAt = I.splitAt

-- vec_sum :: forall a. Num a => Vector a -> a
vec_sum = I.sum

-- vec_tail :: forall a. Vector a -> Vector a
vec_tail = I.tail

-- vec_take :: forall a. Int -> Vector a -> Vector a
vec_take = I.take

-- vec_takeWhile :: forall a. (a -> Bool) -> Vector a -> Vector a
vec_takeWhile = I.takeWhile

-- vec_thaw :: forall a (m :: * -> *). PrimMonad m => Vector a -> m (MVector (PrimState m) a)
vec_thaw = I.thaw

-- vec_toList :: forall a. Vector a -> [a]
vec_toList = I.toList

-- vec_unfoldr :: forall b a. (b -> Maybe (a, b)) -> b -> Vector a
vec_unfoldr = I.unfoldr

-- vec_unfoldrN :: forall b a. Int -> (b -> Maybe (a, b)) -> b -> Vector a
vec_unfoldrN = I.unfoldrN

-- vec_unsafeAccum :: forall a b. (a -> b -> a) -> Vector a -> [(Int, b)] -> Vector a
vec_unsafeAccum = I.unsafeAccum

-- vec_unsafeAccumulate :: forall a b. (a -> b -> a) -> Vector a -> Vector (Int, b) -> Vector a
vec_unsafeAccumulate = I.unsafeAccumulate

-- vec_unsafeAccumulate_ :: forall a b. (a -> b -> a) -> Vector a -> Vector Int -> Vector b -> Vector a
vec_unsafeAccumulate_ = I.unsafeAccumulate_

-- vec_unsafeBackpermute :: forall a. Vector a -> Vector Int -> Vector a
vec_unsafeBackpermute = I.unsafeBackpermute

-- vec_unsafeCopy :: forall (m :: * -> *) a. PrimMonad m => MVector (PrimState m) a -> Vector a -> m ()
vec_unsafeCopy = I.unsafeCopy

-- vec_unsafeDrop :: forall a. Int -> Vector a -> Vector a
vec_unsafeDrop = I.unsafeDrop

-- vec_unsafeFreeze :: forall (m :: * -> *) a. PrimMonad m => MVector (PrimState m) a -> m (Vector a)
vec_unsafeFreeze = I.unsafeFreeze

-- vec_unsafeHead :: forall a. Vector a -> a
vec_unsafeHead = I.unsafeHead

-- vec_unsafeHeadM :: forall a (m :: * -> *). Monad m => Vector a -> m a
vec_unsafeHeadM = I.unsafeHeadM

-- vec_unsafeIndex :: forall a. Vector a -> Int -> a
vec_unsafeIndex = I.unsafeIndex

-- vec_unsafeIndexM :: forall a (m :: * -> *). Monad m => Vector a -> Int -> m a
vec_unsafeIndexM = I.unsafeIndexM

-- vec_unsafeInit :: forall a. Vector a -> Vector a
vec_unsafeInit = I.unsafeInit

-- vec_unsafeLast :: forall a. Vector a -> a
vec_unsafeLast = I.unsafeLast

-- vec_unsafeLastM :: forall a (m :: * -> *). Monad m => Vector a -> m a
vec_unsafeLastM = I.unsafeLastM

-- vec_unsafeSlice :: forall a. Int -> Int -> Vector a -> Vector a
vec_unsafeSlice = I.unsafeSlice

-- vec_unsafeTail :: forall a. Vector a -> Vector a
vec_unsafeTail = I.unsafeTail

-- vec_unsafeTake :: forall a. Int -> Vector a -> Vector a
vec_unsafeTake = I.unsafeTake

-- vec_unsafeThaw :: forall a (m :: * -> *). PrimMonad m => Vector a -> m (MVector (PrimState m) a)
vec_unsafeThaw = I.unsafeThaw

-- vec_unsafeUpd :: forall a. Vector a -> [(Int, a)] -> Vector a
vec_unsafeUpd = I.unsafeUpd

-- vec_unsafeUpdate :: forall a. Vector a -> Vector (Int, a) -> Vector a
vec_unsafeUpdate = I.unsafeUpdate

-- vec_unsafeUpdate_ :: forall a. Vector a -> Vector Int -> Vector a -> Vector a
vec_unsafeUpdate_ = I.unsafeUpdate_

-- vec_unstablePartition :: forall a. (a -> Bool) -> Vector a -> (Vector a, Vector a)
vec_unstablePartition = I.unstablePartition

-- vec_unzip :: forall a b. Vector (a, b) -> (Vector a, Vector b)
vec_unzip = I.unzip

-- vec_unzip3 :: forall a b c. Vector (a, b, c) -> (Vector a, Vector b, Vector c)
vec_unzip3 = I.unzip3

-- vec_unzip4 :: forall a b c d. Vector (a, b, c, d) -> (Vector a, Vector b, Vector c, Vector d)
vec_unzip4 = I.unzip4

-- vec_unzip5 :: forall a b c d e. Vector (a, b, c, d, e) -> (Vector a, Vector b, Vector c, Vector d, Vector e)
vec_unzip5 = I.unzip5

-- vec_unzip6 :: forall a b c d e f. Vector (a, b, c, d, e, f) -> (Vector a, Vector b, Vector c, Vector d, Vector e, Vector f)
vec_unzip6 = I.unzip6

-- vec_update :: forall a. Vector a -> Vector (Int, a) -> Vector a
vec_update = I.update

-- vec_update_ :: forall a. Vector a -> Vector Int -> Vector a -> Vector a
vec_update_ = I.update_

-- vec_zip :: forall a b. Vector a -> Vector b -> Vector (a, b)
vec_zip = I.zip

-- vec_zip3 :: forall a b c. Vector a -> Vector b -> Vector c -> Vector (a, b, c)
vec_zip3 = I.zip3

-- vec_zip4 :: forall a b c d. Vector a -> Vector b -> Vector c -> Vector d -> Vector (a, b, c, d)
vec_zip4 = I.zip4

-- vec_zip5 :: forall a b c d e. Vector a -> Vector b -> Vector c -> Vector d -> Vector e -> Vector (a, b, c, d, e)
vec_zip5 = I.zip5

-- vec_zip6 :: forall a b c d e f. Vector a -> Vector b -> Vector c -> Vector d -> Vector e -> Vector f -> Vector (a, b, c, d, e, f)
vec_zip6 = I.zip6

-- vec_zipWith :: forall a b c. (a -> b -> c) -> Vector a -> Vector b -> Vector c
vec_zipWith = I.zipWith

-- vec_zipWith3 :: forall a b c d. (a -> b -> c -> d) -> Vector a -> Vector b -> Vector c -> Vector d
vec_zipWith3 = I.zipWith3

-- vec_zipWith4 :: forall a b c d e. (a -> b -> c -> d -> e) -> Vector a -> Vector b -> Vector c -> Vector d -> Vector e
vec_zipWith4 = I.zipWith4

-- vec_zipWith5 :: forall a b c d e f. (a -> b -> c -> d -> e -> f) -> Vector a -> Vector b -> Vector c -> Vector d -> Vector e -> Vector f
vec_zipWith5 = I.zipWith5

-- vec_zipWith6 :: forall a b c d e f g. (a -> b -> c -> d -> e -> f -> g) -> Vector a -> Vector b -> Vector c -> Vector d -> Vector e -> Vector f -> Vector g
vec_zipWith6 = I.zipWith6

-- vec_zipWithM :: forall a b (m :: * -> *) c. Monad m => (a -> b -> m c) -> Vector a -> Vector b -> m (Vector c)
vec_zipWithM = I.zipWithM

-- vec_zipWithM_ :: forall a b (m :: * -> *) c. Monad m => (a -> b -> m c) -> Vector a -> Vector b -> m ()
vec_zipWithM_ = I.zipWithM_

-- vec_convert :: forall (v :: * -> *) a (w :: * -> *). (Vector v a, Vector w a) => v a -> w a
vec_convert = I.convert

type VecVector a = I.Vector a

type VecMVector a b = I.MVector a b
