module Data.CaseInsensitive.AsCi where
-- generated by https://github.com/rvion/ride/tree/master/jetpack-gen

import           Data.CaseInsensitive as I

-- ci_map :: forall s1 s2. FoldCase s2 => (s1 -> s2) -> CI s1 -> CI s2
ci_map = I.map

-- ci_mk :: forall s. FoldCase s => s -> CI s
ci_mk = I.mk

type CiCI a = I.CI a
