module Data.HashMap.Strict.AsHm
  ( -- unqualified operators re-export
  (I.!)
  , module Data.HashMap.Strict.AsHm
  ) where
-- generated by https://github.com/rvion/ride/tree/master/jetpack-gen

import qualified Data.HashMap.Strict as I


-- hm_delete :: forall k v. (Eq k, Hashable k) => k -> HashMap k v -> HashMap k v
hm_delete = I.delete

-- hm_difference :: forall k v w. (Eq k, Hashable k) => HashMap k v -> HashMap k w -> HashMap k v
hm_difference = I.difference

-- hm_elems :: forall k v. HashMap k v -> [v]
hm_elems = I.elems

-- hm_empty :: forall k v. HashMap k v
hm_empty = I.empty

-- hm_filter :: forall v k. (v -> Bool) -> HashMap k v -> HashMap k v
hm_filter = I.filter

-- hm_filterWithKey :: forall k v. (k -> v -> Bool) -> HashMap k v -> HashMap k v
hm_filterWithKey = I.filterWithKey

-- hm_foldl' :: forall a v k. (a -> v -> a) -> a -> HashMap k v -> a
hm_foldl' = I.foldl'

-- hm_foldlWithKey' :: forall a k v. (a -> k -> v -> a) -> a -> HashMap k v -> a
hm_foldlWithKey' = I.foldlWithKey'

-- hm_foldr :: forall v a k. (v -> a -> a) -> a -> HashMap k v -> a
hm_foldr = I.foldr

-- hm_foldrWithKey :: forall k v a. (k -> v -> a -> a) -> a -> HashMap k v -> a
hm_foldrWithKey = I.foldrWithKey

-- hm_intersection :: forall k v w. (Eq k, Hashable k) => HashMap k v -> HashMap k w -> HashMap k v
hm_intersection = I.intersection

-- hm_keys :: forall k v. HashMap k v -> [k]
hm_keys = I.keys

-- hm_lookup :: forall k v. (Eq k, Hashable k) => k -> HashMap k v -> Maybe v
hm_lookup = I.lookup

-- hm_lookupDefault :: forall v k. (Eq k, Hashable k) => v -> k -> HashMap k v -> v
hm_lookupDefault = I.lookupDefault

-- hm_member :: forall k a. (Eq k, Hashable k) => k -> HashMap k a -> Bool
hm_member = I.member

-- hm_null :: forall k v. HashMap k v -> Bool
hm_null = I.null

-- hm_size :: forall k v. HashMap k v -> Int
hm_size = I.size

-- hm_toList :: forall k v. HashMap k v -> [(k, v)]
hm_toList = I.toList

-- hm_traverseWithKey :: forall k v1 (f :: * -> *) v2. Applicative f => (k -> v1 -> f v2) -> HashMap k v1 -> f (HashMap k v2)
hm_traverseWithKey = I.traverseWithKey

-- hm_union :: forall k v. (Eq k, Hashable k) => HashMap k v -> HashMap k v -> HashMap k v
hm_union = I.union

-- hm_unions :: forall k v. (Eq k, Hashable k) => [HashMap k v] -> HashMap k v
hm_unions = I.unions

-- hm_adjust :: forall v k. (Eq k, Hashable k) => (v -> v) -> k -> HashMap k v -> HashMap k v
hm_adjust = I.adjust

-- hm_fromList :: forall k v. (Eq k, Hashable k) => [(k, v)] -> HashMap k v
hm_fromList = I.fromList

-- hm_fromListWith :: forall v k. (Eq k, Hashable k) => (v -> v -> v) -> [(k, v)] -> HashMap k v
hm_fromListWith = I.fromListWith

-- hm_insert :: forall k v. (Eq k, Hashable k) => k -> v -> HashMap k v -> HashMap k v
hm_insert = I.insert

-- hm_insertWith :: forall v k. (Eq k, Hashable k) => (v -> v -> v) -> k -> v -> HashMap k v -> HashMap k v
hm_insertWith = I.insertWith

-- hm_intersectionWith :: forall v1 v2 v3 k. (Eq k, Hashable k) => (v1 -> v2 -> v3) -> HashMap k v1 -> HashMap k v2 -> HashMap k v3
hm_intersectionWith = I.intersectionWith

-- hm_map :: forall v1 v2 k. (v1 -> v2) -> HashMap k v1 -> HashMap k v2
hm_map = I.map

-- hm_mapWithKey :: forall k v1 v2. (k -> v1 -> v2) -> HashMap k v1 -> HashMap k v2
hm_mapWithKey = I.mapWithKey

-- hm_singleton :: forall k v. Hashable k => k -> v -> HashMap k v
hm_singleton = I.singleton

-- hm_unionWith :: forall v k. (Eq k, Hashable k) => (v -> v -> v) -> HashMap k v -> HashMap k v -> HashMap k v
hm_unionWith = I.unionWith

type HmHashMap a b = I.HashMap a b
