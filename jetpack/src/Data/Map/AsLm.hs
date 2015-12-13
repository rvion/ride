module Data.Map.AsLm where
-- generated by rvion/jetpack-gen 

import Data.Map as I

-- lm_fold :: forall a b k. (a -> b -> b) -> b -> Map k a -> b
lm_fold = I.fold
-- lm_foldWithKey :: forall k a b. (k -> a -> b -> b) -> b -> Map k a -> b
lm_foldWithKey = I.foldWithKey
-- lm_insertLookupWithKey' :: forall k a.
Ord k =>
(k -> a -> a -> a) -> k -> a -> Map k a -> (Maybe a, Map k a)
lm_insertLookupWithKey' = I.insertLookupWithKey'
-- lm_insertWith' :: forall a k. Ord k => (a -> a -> a) -> k -> a -> Map k a -> Map k a
lm_insertWith' = I.insertWith'
-- lm_insertWithKey' :: forall k a.
Ord k =>
(k -> a -> a -> a) -> k -> a -> Map k a -> Map k a
lm_insertWithKey' = I.insertWithKey'
-- (!) :: forall k a. Ord k => Map k a -> k -> a
(!) = (I.!)
-- (\\) :: forall k a b. Ord k => Map k a -> Map k b -> Map k a
(\\) = (I.\\)
-- lm_adjust :: forall a k. Ord k => (a -> a) -> k -> Map k a -> Map k a
lm_adjust = I.adjust
-- lm_adjustWithKey :: forall k a. Ord k => (k -> a -> a) -> k -> Map k a -> Map k a
lm_adjustWithKey = I.adjustWithKey
-- lm_alter :: forall a k.
Ord k =>
(Maybe a -> Maybe a) -> k -> Map k a -> Map k a
lm_alter = I.alter
-- lm_assocs :: forall k a. Map k a -> [(k, a)]
lm_assocs = I.assocs
-- lm_delete :: forall k a. Ord k => k -> Map k a -> Map k a
lm_delete = I.delete
-- lm_deleteAt :: forall k a. Int -> Map k a -> Map k a
lm_deleteAt = I.deleteAt
-- lm_deleteFindMax :: forall k a. Map k a -> ((k, a), Map k a)
lm_deleteFindMax = I.deleteFindMax
-- lm_deleteFindMin :: forall k a. Map k a -> ((k, a), Map k a)
lm_deleteFindMin = I.deleteFindMin
-- lm_deleteMax :: forall k a. Map k a -> Map k a
lm_deleteMax = I.deleteMax
-- lm_deleteMin :: forall k a. Map k a -> Map k a
lm_deleteMin = I.deleteMin
-- lm_difference :: forall k a b. Ord k => Map k a -> Map k b -> Map k a
lm_difference = I.difference
-- lm_differenceWith :: forall a b k.
Ord k =>
(a -> b -> Maybe a) -> Map k a -> Map k b -> Map k a
lm_differenceWith = I.differenceWith
-- lm_differenceWithKey :: forall k a b.
Ord k =>
(k -> a -> b -> Maybe a) -> Map k a -> Map k b -> Map k a
lm_differenceWithKey = I.differenceWithKey
-- lm_elemAt :: forall k a. Int -> Map k a -> (k, a)
lm_elemAt = I.elemAt
-- lm_elems :: forall k a. Map k a -> [a]
lm_elems = I.elems
-- lm_empty :: forall k a. Map k a
lm_empty = I.empty
-- lm_filter :: forall a k. (a -> Bool) -> Map k a -> Map k a
lm_filter = I.filter
-- lm_filterWithKey :: forall k a. (k -> a -> Bool) -> Map k a -> Map k a
lm_filterWithKey = I.filterWithKey
-- lm_findIndex :: forall k a. Ord k => k -> Map k a -> Int
lm_findIndex = I.findIndex
-- lm_findMax :: forall k a. Map k a -> (k, a)
lm_findMax = I.findMax
-- lm_findMin :: forall k a. Map k a -> (k, a)
lm_findMin = I.findMin
-- lm_findWithDefault :: forall a k. Ord k => a -> k -> Map k a -> a
lm_findWithDefault = I.findWithDefault
-- lm_foldMapWithKey :: forall k a m. Monoid m => (k -> a -> m) -> Map k a -> m
lm_foldMapWithKey = I.foldMapWithKey
-- lm_foldl :: forall a b k. (a -> b -> a) -> a -> Map k b -> a
lm_foldl = I.foldl
-- lm_foldl' :: forall a b k. (a -> b -> a) -> a -> Map k b -> a
lm_foldl' = I.foldl'
-- lm_foldlWithKey :: forall a k b. (a -> k -> b -> a) -> a -> Map k b -> a
lm_foldlWithKey = I.foldlWithKey
-- lm_foldlWithKey' :: forall a k b. (a -> k -> b -> a) -> a -> Map k b -> a
lm_foldlWithKey' = I.foldlWithKey'
-- lm_foldr :: forall a b k. (a -> b -> b) -> b -> Map k a -> b
lm_foldr = I.foldr
-- lm_foldr' :: forall a b k. (a -> b -> b) -> b -> Map k a -> b
lm_foldr' = I.foldr'
-- lm_foldrWithKey :: forall k a b. (k -> a -> b -> b) -> b -> Map k a -> b
lm_foldrWithKey = I.foldrWithKey
-- lm_foldrWithKey' :: forall k a b. (k -> a -> b -> b) -> b -> Map k a -> b
lm_foldrWithKey' = I.foldrWithKey'
-- lm_fromAscList :: forall k a. Eq k => [(k, a)] -> Map k a
lm_fromAscList = I.fromAscList
-- lm_fromAscListWith :: forall a k. Eq k => (a -> a -> a) -> [(k, a)] -> Map k a
lm_fromAscListWith = I.fromAscListWith
-- lm_fromAscListWithKey :: forall k a. Eq k => (k -> a -> a -> a) -> [(k, a)] -> Map k a
lm_fromAscListWithKey = I.fromAscListWithKey
-- lm_fromDistinctAscList :: forall k a. [(k, a)] -> Map k a
lm_fromDistinctAscList = I.fromDistinctAscList
-- lm_fromList :: forall k a. Ord k => [(k, a)] -> Map k a
lm_fromList = I.fromList
-- lm_fromListWith :: forall a k. Ord k => (a -> a -> a) -> [(k, a)] -> Map k a
lm_fromListWith = I.fromListWith
-- lm_fromListWithKey :: forall k a. Ord k => (k -> a -> a -> a) -> [(k, a)] -> Map k a
lm_fromListWithKey = I.fromListWithKey
-- lm_fromSet :: forall k a. (k -> a) -> Set k -> Map k a
lm_fromSet = I.fromSet
-- lm_insert :: forall k a. Ord k => k -> a -> Map k a -> Map k a
lm_insert = I.insert
-- lm_insertLookupWithKey :: forall k a.
Ord k =>
(k -> a -> a -> a) -> k -> a -> Map k a -> (Maybe a, Map k a)
lm_insertLookupWithKey = I.insertLookupWithKey
-- lm_insertWith :: forall a k. Ord k => (a -> a -> a) -> k -> a -> Map k a -> Map k a
lm_insertWith = I.insertWith
-- lm_insertWithKey :: forall k a.
Ord k =>
(k -> a -> a -> a) -> k -> a -> Map k a -> Map k a
lm_insertWithKey = I.insertWithKey
-- lm_intersection :: forall k a b. Ord k => Map k a -> Map k b -> Map k a
lm_intersection = I.intersection
-- lm_intersectionWith :: forall a b c k.
Ord k =>
(a -> b -> c) -> Map k a -> Map k b -> Map k c
lm_intersectionWith = I.intersectionWith
-- lm_intersectionWithKey :: forall k a b c.
Ord k =>
(k -> a -> b -> c) -> Map k a -> Map k b -> Map k c
lm_intersectionWithKey = I.intersectionWithKey
-- lm_isProperSubmapOf :: forall k a. (Ord k, Eq a) => Map k a -> Map k a -> Bool
lm_isProperSubmapOf = I.isProperSubmapOf
-- lm_isProperSubmapOfBy :: forall a b k.
Ord k =>
(a -> b -> Bool) -> Map k a -> Map k b -> Bool
lm_isProperSubmapOfBy = I.isProperSubmapOfBy
-- lm_isSubmapOf :: forall k a. (Ord k, Eq a) => Map k a -> Map k a -> Bool
lm_isSubmapOf = I.isSubmapOf
-- lm_isSubmapOfBy :: forall a b k.
Ord k =>
(a -> b -> Bool) -> Map k a -> Map k b -> Bool
lm_isSubmapOfBy = I.isSubmapOfBy
-- lm_keys :: forall k a. Map k a -> [k]
lm_keys = I.keys
-- lm_keysSet :: forall k a. Map k a -> Set k
lm_keysSet = I.keysSet
-- lm_lookup :: forall k a. Ord k => k -> Map k a -> Maybe a
lm_lookup = I.lookup
-- lm_lookupGE :: forall k v. Ord k => k -> Map k v -> Maybe (k, v)
lm_lookupGE = I.lookupGE
-- lm_lookupGT :: forall k v. Ord k => k -> Map k v -> Maybe (k, v)
lm_lookupGT = I.lookupGT
-- lm_lookupIndex :: forall k a. Ord k => k -> Map k a -> Maybe Int
lm_lookupIndex = I.lookupIndex
-- lm_lookupLE :: forall k v. Ord k => k -> Map k v -> Maybe (k, v)
lm_lookupLE = I.lookupLE
-- lm_lookupLT :: forall k v. Ord k => k -> Map k v -> Maybe (k, v)
lm_lookupLT = I.lookupLT
-- lm_map :: forall a b k. (a -> b) -> Map k a -> Map k b
lm_map = I.map
-- lm_mapAccum :: forall a b c k. (a -> b -> (a, c)) -> a -> Map k b -> (a, Map k c)
lm_mapAccum = I.mapAccum
-- lm_mapAccumRWithKey :: forall a k b c.
(a -> k -> b -> (a, c)) -> a -> Map k b -> (a, Map k c)
lm_mapAccumRWithKey = I.mapAccumRWithKey
-- lm_mapAccumWithKey :: forall a k b c.
(a -> k -> b -> (a, c)) -> a -> Map k b -> (a, Map k c)
lm_mapAccumWithKey = I.mapAccumWithKey
-- lm_mapEither :: forall a b c k. (a -> Either b c) -> Map k a -> (Map k b, Map k c)
lm_mapEither = I.mapEither
-- lm_mapEitherWithKey :: forall k a b c.
(k -> a -> Either b c) -> Map k a -> (Map k b, Map k c)
lm_mapEitherWithKey = I.mapEitherWithKey
-- lm_mapKeys :: forall k1 k2 a. Ord k2 => (k1 -> k2) -> Map k1 a -> Map k2 a
lm_mapKeys = I.mapKeys
-- lm_mapKeysMonotonic :: forall k1 k2 a. (k1 -> k2) -> Map k1 a -> Map k2 a
lm_mapKeysMonotonic = I.mapKeysMonotonic
-- lm_mapKeysWith :: forall a k1 k2.
Ord k2 =>
(a -> a -> a) -> (k1 -> k2) -> Map k1 a -> Map k2 a
lm_mapKeysWith = I.mapKeysWith
-- lm_mapMaybe :: forall a b k. (a -> Maybe b) -> Map k a -> Map k b
lm_mapMaybe = I.mapMaybe
-- lm_mapMaybeWithKey :: forall k a b. (k -> a -> Maybe b) -> Map k a -> Map k b
lm_mapMaybeWithKey = I.mapMaybeWithKey
-- lm_mapWithKey :: forall k a b. (k -> a -> b) -> Map k a -> Map k b
lm_mapWithKey = I.mapWithKey
-- lm_maxView :: forall k a. Map k a -> Maybe (a, Map k a)
lm_maxView = I.maxView
-- lm_maxViewWithKey :: forall k a. Map k a -> Maybe ((k, a), Map k a)
lm_maxViewWithKey = I.maxViewWithKey
-- lm_member :: forall k a. Ord k => k -> Map k a -> Bool
lm_member = I.member
-- lm_mergeWithKey :: forall k a b c.
Ord k =>
(k -> a -> b -> Maybe c)
-> (Map k a -> Map k c)
-> (Map k b -> Map k c)
-> Map k a
-> Map k b
-> Map k c
lm_mergeWithKey = I.mergeWithKey
-- lm_minView :: forall k a. Map k a -> Maybe (a, Map k a)
lm_minView = I.minView
-- lm_minViewWithKey :: forall k a. Map k a -> Maybe ((k, a), Map k a)
lm_minViewWithKey = I.minViewWithKey
-- lm_notMember :: forall k a. Ord k => k -> Map k a -> Bool
lm_notMember = I.notMember
-- lm_null :: forall k a. Map k a -> Bool
lm_null = I.null
-- lm_partition :: forall a k. (a -> Bool) -> Map k a -> (Map k a, Map k a)
lm_partition = I.partition
-- lm_partitionWithKey :: forall k a. (k -> a -> Bool) -> Map k a -> (Map k a, Map k a)
lm_partitionWithKey = I.partitionWithKey
-- lm_showTree :: forall k a. (Show k, Show a) => Map k a -> String
lm_showTree = I.showTree
-- lm_showTreeWith :: forall k a. (k -> a -> String) -> Bool -> Bool -> Map k a -> String
lm_showTreeWith = I.showTreeWith
-- lm_singleton :: forall k a. k -> a -> Map k a
lm_singleton = I.singleton
-- lm_size :: forall k a. Map k a -> Int
lm_size = I.size
-- lm_split :: forall k a. Ord k => k -> Map k a -> (Map k a, Map k a)
lm_split = I.split
-- lm_splitLookup :: forall k a. Ord k => k -> Map k a -> (Map k a, Maybe a, Map k a)
lm_splitLookup = I.splitLookup
-- lm_splitRoot :: forall k b. Map k b -> [Map k b]
lm_splitRoot = I.splitRoot
-- lm_toAscList :: forall k a. Map k a -> [(k, a)]
lm_toAscList = I.toAscList
-- lm_toDescList :: forall k a. Map k a -> [(k, a)]
lm_toDescList = I.toDescList
-- lm_toList :: forall k a. Map k a -> [(k, a)]
lm_toList = I.toList
-- lm_traverseWithKey :: forall k a (t :: * -> *) b.
Applicative t =>
(k -> a -> t b) -> Map k a -> t (Map k b)
lm_traverseWithKey = I.traverseWithKey
-- lm_union :: forall k a. Ord k => Map k a -> Map k a -> Map k a
lm_union = I.union
-- lm_unionWith :: forall a k. Ord k => (a -> a -> a) -> Map k a -> Map k a -> Map k a
lm_unionWith = I.unionWith
-- lm_unionWithKey :: forall k a.
Ord k =>
(k -> a -> a -> a) -> Map k a -> Map k a -> Map k a
lm_unionWithKey = I.unionWithKey
-- lm_unions :: forall k a. Ord k => [Map k a] -> Map k a
lm_unions = I.unions
-- lm_unionsWith :: forall a k. Ord k => (a -> a -> a) -> [Map k a] -> Map k a
lm_unionsWith = I.unionsWith
-- lm_update :: forall a k. Ord k => (a -> Maybe a) -> k -> Map k a -> Map k a
lm_update = I.update
-- lm_updateAt :: forall k a. (k -> a -> Maybe a) -> Int -> Map k a -> Map k a
lm_updateAt = I.updateAt
-- lm_updateLookupWithKey :: forall k a.
Ord k =>
(k -> a -> Maybe a) -> k -> Map k a -> (Maybe a, Map k a)
lm_updateLookupWithKey = I.updateLookupWithKey
-- lm_updateMax :: forall a k. (a -> Maybe a) -> Map k a -> Map k a
lm_updateMax = I.updateMax
-- lm_updateMaxWithKey :: forall k a. (k -> a -> Maybe a) -> Map k a -> Map k a
lm_updateMaxWithKey = I.updateMaxWithKey
-- lm_updateMin :: forall a k. (a -> Maybe a) -> Map k a -> Map k a
lm_updateMin = I.updateMin
-- lm_updateMinWithKey :: forall k a. (k -> a -> Maybe a) -> Map k a -> Map k a
lm_updateMinWithKey = I.updateMinWithKey
-- lm_updateWithKey :: forall k a. Ord k => (k -> a -> Maybe a) -> k -> Map k a -> Map k a
lm_updateWithKey = I.updateWithKey
-- lm_valid :: forall k a. Ord k => Map k a -> Bool
lm_valid = I.valid
type LmMap a b = I.Map a b
