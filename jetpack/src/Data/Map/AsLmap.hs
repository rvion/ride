module Data.Map.AsLmap where
-- generated by https://github.com/rvion/ride/tree/master/jetpack-gen

import Data.Map as I

-- lmap_fold :: forall a b k. (a -> b -> b) -> b -> Map k a -> b
lmap_fold = I.fold

-- lmap_foldWithKey :: forall k a b. (k -> a -> b -> b) -> b -> Map k a -> b
lmap_foldWithKey = I.foldWithKey

-- lmap_insertLookupWithKey' :: forall k a. Ord k => (k -> a -> a -> a) -> k -> a -> Map k a -> (Maybe a, Map k a)
lmap_insertLookupWithKey' = I.insertLookupWithKey'

-- lmap_insertWith' :: forall a k. Ord k => (a -> a -> a) -> k -> a -> Map k a -> Map k a
lmap_insertWith' = I.insertWith'

-- lmap_insertWithKey' :: forall k a. Ord k => (k -> a -> a -> a) -> k -> a -> Map k a -> Map k a
lmap_insertWithKey' = I.insertWithKey'

-- lmap_adjust :: forall a k. Ord k => (a -> a) -> k -> Map k a -> Map k a
lmap_adjust = I.adjust

-- lmap_adjustWithKey :: forall k a. Ord k => (k -> a -> a) -> k -> Map k a -> Map k a
lmap_adjustWithKey = I.adjustWithKey

-- lmap_alter :: forall a k. Ord k => (Maybe a -> Maybe a) -> k -> Map k a -> Map k a
lmap_alter = I.alter

-- lmap_assocs :: forall k a. Map k a -> [(k, a)]
lmap_assocs = I.assocs

-- lmap_delete :: forall k a. Ord k => k -> Map k a -> Map k a
lmap_delete = I.delete

-- lmap_deleteAt :: forall k a. Int -> Map k a -> Map k a
lmap_deleteAt = I.deleteAt

-- lmap_deleteFindMax :: forall k a. Map k a -> ((k, a), Map k a)
lmap_deleteFindMax = I.deleteFindMax

-- lmap_deleteFindMin :: forall k a. Map k a -> ((k, a), Map k a)
lmap_deleteFindMin = I.deleteFindMin

-- lmap_deleteMax :: forall k a. Map k a -> Map k a
lmap_deleteMax = I.deleteMax

-- lmap_deleteMin :: forall k a. Map k a -> Map k a
lmap_deleteMin = I.deleteMin

-- lmap_difference :: forall k a b. Ord k => Map k a -> Map k b -> Map k a
lmap_difference = I.difference

-- lmap_differenceWith :: forall a b k. Ord k => (a -> b -> Maybe a) -> Map k a -> Map k b -> Map k a
lmap_differenceWith = I.differenceWith

-- lmap_differenceWithKey :: forall k a b. Ord k => (k -> a -> b -> Maybe a) -> Map k a -> Map k b -> Map k a
lmap_differenceWithKey = I.differenceWithKey

-- lmap_elemAt :: forall k a. Int -> Map k a -> (k, a)
lmap_elemAt = I.elemAt

-- lmap_elems :: forall k a. Map k a -> [a]
lmap_elems = I.elems

-- lmap_empty :: forall k a. Map k a
lmap_empty = I.empty

-- lmap_filter :: forall a k. (a -> Bool) -> Map k a -> Map k a
lmap_filter = I.filter

-- lmap_filterWithKey :: forall k a. (k -> a -> Bool) -> Map k a -> Map k a
lmap_filterWithKey = I.filterWithKey

-- lmap_findIndex :: forall k a. Ord k => k -> Map k a -> Int
lmap_findIndex = I.findIndex

-- lmap_findMax :: forall k a. Map k a -> (k, a)
lmap_findMax = I.findMax

-- lmap_findMin :: forall k a. Map k a -> (k, a)
lmap_findMin = I.findMin

-- lmap_findWithDefault :: forall a k. Ord k => a -> k -> Map k a -> a
lmap_findWithDefault = I.findWithDefault

-- lmap_foldMapWithKey :: forall k a m. Monoid m => (k -> a -> m) -> Map k a -> m
lmap_foldMapWithKey = I.foldMapWithKey

-- lmap_foldl :: forall a b k. (a -> b -> a) -> a -> Map k b -> a
lmap_foldl = I.foldl

-- lmap_foldl' :: forall a b k. (a -> b -> a) -> a -> Map k b -> a
lmap_foldl' = I.foldl'

-- lmap_foldlWithKey :: forall a k b. (a -> k -> b -> a) -> a -> Map k b -> a
lmap_foldlWithKey = I.foldlWithKey

-- lmap_foldlWithKey' :: forall a k b. (a -> k -> b -> a) -> a -> Map k b -> a
lmap_foldlWithKey' = I.foldlWithKey'

-- lmap_foldr :: forall a b k. (a -> b -> b) -> b -> Map k a -> b
lmap_foldr = I.foldr

-- lmap_foldr' :: forall a b k. (a -> b -> b) -> b -> Map k a -> b
lmap_foldr' = I.foldr'

-- lmap_foldrWithKey :: forall k a b. (k -> a -> b -> b) -> b -> Map k a -> b
lmap_foldrWithKey = I.foldrWithKey

-- lmap_foldrWithKey' :: forall k a b. (k -> a -> b -> b) -> b -> Map k a -> b
lmap_foldrWithKey' = I.foldrWithKey'

-- lmap_fromAscList :: forall k a. Eq k => [(k, a)] -> Map k a
lmap_fromAscList = I.fromAscList

-- lmap_fromAscListWith :: forall a k. Eq k => (a -> a -> a) -> [(k, a)] -> Map k a
lmap_fromAscListWith = I.fromAscListWith

-- lmap_fromAscListWithKey :: forall k a. Eq k => (k -> a -> a -> a) -> [(k, a)] -> Map k a
lmap_fromAscListWithKey = I.fromAscListWithKey

-- lmap_fromDistinctAscList :: forall k a. [(k, a)] -> Map k a
lmap_fromDistinctAscList = I.fromDistinctAscList

-- lmap_fromList :: forall k a. Ord k => [(k, a)] -> Map k a
lmap_fromList = I.fromList

-- lmap_fromListWith :: forall a k. Ord k => (a -> a -> a) -> [(k, a)] -> Map k a
lmap_fromListWith = I.fromListWith

-- lmap_fromListWithKey :: forall k a. Ord k => (k -> a -> a -> a) -> [(k, a)] -> Map k a
lmap_fromListWithKey = I.fromListWithKey

-- lmap_fromSet :: forall k a. (k -> a) -> Set k -> Map k a
lmap_fromSet = I.fromSet

-- lmap_insert :: forall k a. Ord k => k -> a -> Map k a -> Map k a
lmap_insert = I.insert

-- lmap_insertLookupWithKey :: forall k a. Ord k => (k -> a -> a -> a) -> k -> a -> Map k a -> (Maybe a, Map k a)
lmap_insertLookupWithKey = I.insertLookupWithKey

-- lmap_insertWith :: forall a k. Ord k => (a -> a -> a) -> k -> a -> Map k a -> Map k a
lmap_insertWith = I.insertWith

-- lmap_insertWithKey :: forall k a. Ord k => (k -> a -> a -> a) -> k -> a -> Map k a -> Map k a
lmap_insertWithKey = I.insertWithKey

-- lmap_intersection :: forall k a b. Ord k => Map k a -> Map k b -> Map k a
lmap_intersection = I.intersection

-- lmap_intersectionWith :: forall a b c k. Ord k => (a -> b -> c) -> Map k a -> Map k b -> Map k c
lmap_intersectionWith = I.intersectionWith

-- lmap_intersectionWithKey :: forall k a b c. Ord k => (k -> a -> b -> c) -> Map k a -> Map k b -> Map k c
lmap_intersectionWithKey = I.intersectionWithKey

-- lmap_isProperSubmapOf :: forall k a. (Ord k, Eq a) => Map k a -> Map k a -> Bool
lmap_isProperSubmapOf = I.isProperSubmapOf

-- lmap_isProperSubmapOfBy :: forall a b k. Ord k => (a -> b -> Bool) -> Map k a -> Map k b -> Bool
lmap_isProperSubmapOfBy = I.isProperSubmapOfBy

-- lmap_isSubmapOf :: forall k a. (Ord k, Eq a) => Map k a -> Map k a -> Bool
lmap_isSubmapOf = I.isSubmapOf

-- lmap_isSubmapOfBy :: forall a b k. Ord k => (a -> b -> Bool) -> Map k a -> Map k b -> Bool
lmap_isSubmapOfBy = I.isSubmapOfBy

-- lmap_keys :: forall k a. Map k a -> [k]
lmap_keys = I.keys

-- lmap_keysSet :: forall k a. Map k a -> Set k
lmap_keysSet = I.keysSet

-- lmap_lookup :: forall k a. Ord k => k -> Map k a -> Maybe a
lmap_lookup = I.lookup

-- lmap_lookupGE :: forall k v. Ord k => k -> Map k v -> Maybe (k, v)
lmap_lookupGE = I.lookupGE

-- lmap_lookupGT :: forall k v. Ord k => k -> Map k v -> Maybe (k, v)
lmap_lookupGT = I.lookupGT

-- lmap_lookupIndex :: forall k a. Ord k => k -> Map k a -> Maybe Int
lmap_lookupIndex = I.lookupIndex

-- lmap_lookupLE :: forall k v. Ord k => k -> Map k v -> Maybe (k, v)
lmap_lookupLE = I.lookupLE

-- lmap_lookupLT :: forall k v. Ord k => k -> Map k v -> Maybe (k, v)
lmap_lookupLT = I.lookupLT

-- lmap_map :: forall a b k. (a -> b) -> Map k a -> Map k b
lmap_map = I.map

-- lmap_mapAccum :: forall a b c k. (a -> b -> (a, c)) -> a -> Map k b -> (a, Map k c)
lmap_mapAccum = I.mapAccum

-- lmap_mapAccumRWithKey :: forall a k b c. (a -> k -> b -> (a, c)) -> a -> Map k b -> (a, Map k c)
lmap_mapAccumRWithKey = I.mapAccumRWithKey

-- lmap_mapAccumWithKey :: forall a k b c. (a -> k -> b -> (a, c)) -> a -> Map k b -> (a, Map k c)
lmap_mapAccumWithKey = I.mapAccumWithKey

-- lmap_mapEither :: forall a b c k. (a -> Either b c) -> Map k a -> (Map k b, Map k c)
lmap_mapEither = I.mapEither

-- lmap_mapEitherWithKey :: forall k a b c. (k -> a -> Either b c) -> Map k a -> (Map k b, Map k c)
lmap_mapEitherWithKey = I.mapEitherWithKey

-- lmap_mapKeys :: forall k1 k2 a. Ord k2 => (k1 -> k2) -> Map k1 a -> Map k2 a
lmap_mapKeys = I.mapKeys

-- lmap_mapKeysMonotonic :: forall k1 k2 a. (k1 -> k2) -> Map k1 a -> Map k2 a
lmap_mapKeysMonotonic = I.mapKeysMonotonic

-- lmap_mapKeysWith :: forall a k1 k2. Ord k2 => (a -> a -> a) -> (k1 -> k2) -> Map k1 a -> Map k2 a
lmap_mapKeysWith = I.mapKeysWith

-- lmap_mapMaybe :: forall a b k. (a -> Maybe b) -> Map k a -> Map k b
lmap_mapMaybe = I.mapMaybe

-- lmap_mapMaybeWithKey :: forall k a b. (k -> a -> Maybe b) -> Map k a -> Map k b
lmap_mapMaybeWithKey = I.mapMaybeWithKey

-- lmap_mapWithKey :: forall k a b. (k -> a -> b) -> Map k a -> Map k b
lmap_mapWithKey = I.mapWithKey

-- lmap_maxView :: forall k a. Map k a -> Maybe (a, Map k a)
lmap_maxView = I.maxView

-- lmap_maxViewWithKey :: forall k a. Map k a -> Maybe ((k, a), Map k a)
lmap_maxViewWithKey = I.maxViewWithKey

-- lmap_member :: forall k a. Ord k => k -> Map k a -> Bool
lmap_member = I.member

-- lmap_mergeWithKey :: forall k a b c. Ord k => (k -> a -> b -> Maybe c) -> (Map k a -> Map k c) -> (Map k b -> Map k c) -> Map k a -> Map k b -> Map k c
lmap_mergeWithKey = I.mergeWithKey

-- lmap_minView :: forall k a. Map k a -> Maybe (a, Map k a)
lmap_minView = I.minView

-- lmap_minViewWithKey :: forall k a. Map k a -> Maybe ((k, a), Map k a)
lmap_minViewWithKey = I.minViewWithKey

-- lmap_notMember :: forall k a. Ord k => k -> Map k a -> Bool
lmap_notMember = I.notMember

-- lmap_null :: forall k a. Map k a -> Bool
lmap_null = I.null

-- lmap_partition :: forall a k. (a -> Bool) -> Map k a -> (Map k a, Map k a)
lmap_partition = I.partition

-- lmap_partitionWithKey :: forall k a. (k -> a -> Bool) -> Map k a -> (Map k a, Map k a)
lmap_partitionWithKey = I.partitionWithKey

-- lmap_showTree :: forall k a. (Show k, Show a) => Map k a -> String
lmap_showTree = I.showTree

-- lmap_showTreeWith :: forall k a. (k -> a -> String) -> Bool -> Bool -> Map k a -> String
lmap_showTreeWith = I.showTreeWith

-- lmap_singleton :: forall k a. k -> a -> Map k a
lmap_singleton = I.singleton

-- lmap_size :: forall k a. Map k a -> Int
lmap_size = I.size

-- lmap_split :: forall k a. Ord k => k -> Map k a -> (Map k a, Map k a)
lmap_split = I.split

-- lmap_splitLookup :: forall k a. Ord k => k -> Map k a -> (Map k a, Maybe a, Map k a)
lmap_splitLookup = I.splitLookup

-- lmap_splitRoot :: forall k b. Map k b -> [Map k b]
lmap_splitRoot = I.splitRoot

-- lmap_toAscList :: forall k a. Map k a -> [(k, a)]
lmap_toAscList = I.toAscList

-- lmap_toDescList :: forall k a. Map k a -> [(k, a)]
lmap_toDescList = I.toDescList

-- lmap_toList :: forall k a. Map k a -> [(k, a)]
lmap_toList = I.toList

-- lmap_traverseWithKey :: forall k a (t :: * -> *) b. Applicative t => (k -> a -> t b) -> Map k a -> t (Map k b)
lmap_traverseWithKey = I.traverseWithKey

-- lmap_union :: forall k a. Ord k => Map k a -> Map k a -> Map k a
lmap_union = I.union

-- lmap_unionWith :: forall a k. Ord k => (a -> a -> a) -> Map k a -> Map k a -> Map k a
lmap_unionWith = I.unionWith

-- lmap_unionWithKey :: forall k a. Ord k => (k -> a -> a -> a) -> Map k a -> Map k a -> Map k a
lmap_unionWithKey = I.unionWithKey

-- lmap_unions :: forall k a. Ord k => [Map k a] -> Map k a
lmap_unions = I.unions

-- lmap_unionsWith :: forall a k. Ord k => (a -> a -> a) -> [Map k a] -> Map k a
lmap_unionsWith = I.unionsWith

-- lmap_update :: forall a k. Ord k => (a -> Maybe a) -> k -> Map k a -> Map k a
lmap_update = I.update

-- lmap_updateAt :: forall k a. (k -> a -> Maybe a) -> Int -> Map k a -> Map k a
lmap_updateAt = I.updateAt

-- lmap_updateLookupWithKey :: forall k a. Ord k => (k -> a -> Maybe a) -> k -> Map k a -> (Maybe a, Map k a)
lmap_updateLookupWithKey = I.updateLookupWithKey

-- lmap_updateMax :: forall a k. (a -> Maybe a) -> Map k a -> Map k a
lmap_updateMax = I.updateMax

-- lmap_updateMaxWithKey :: forall k a. (k -> a -> Maybe a) -> Map k a -> Map k a
lmap_updateMaxWithKey = I.updateMaxWithKey

-- lmap_updateMin :: forall a k. (a -> Maybe a) -> Map k a -> Map k a
lmap_updateMin = I.updateMin

-- lmap_updateMinWithKey :: forall k a. (k -> a -> Maybe a) -> Map k a -> Map k a
lmap_updateMinWithKey = I.updateMinWithKey

-- lmap_updateWithKey :: forall k a. Ord k => (k -> a -> Maybe a) -> k -> Map k a -> Map k a
lmap_updateWithKey = I.updateWithKey

-- lmap_valid :: forall k a. Ord k => Map k a -> Bool
lmap_valid = I.valid

type LmapMap a b = I.Map a b
