# jetpack-gen:

:warning: WIP
Please, __don't post anything on reddit for now__.

jetpack-gen is a tool that generate 

```haskell
module Data.Map.Strict.AsM where
-- generated by https://github.com/rvion/ride/tree/master/jetpack-gen

import Data.Map.Strict as I

-- (!) :: forall k a. Ord k => Map k a -> k -> a
(!) = (I.!)

-- (\\) :: forall k a b. Ord k => Map k a -> Map k b -> Map k a
(\\) = (I.\\)

-- m_assocs :: forall k a. Map k a -> [(k, a)]
m_assocs = I.assocs

-- m_delete :: forall k a. Ord k => k -> Map k a -> Map k a
m_delete = I.delete

-- m_deleteAt :: forall k a. Int -> Map k a -> Map k a
m_deleteAt = I.deleteAt

-- m_deleteFindMax :: forall k a. Map k a -> ((k, a), Map k a)
m_deleteFindMax = I.deleteFindMax

[...]
```

from this line:

```
m Data.Map.Strict
```

## Usages

 - per project framework including all functions
 - generate practical export bindings for a local project

## Necessary Extensions

 - MagicHash to be able to reexport symbols ending with a Hash (like t_unpackCString# from Data.Text)
 - rankNtypes to handle complicated types found in `Conduit` or `Microlens`
 - DataKinds to be able to reexport prefixed promoted data kinds
 - FlexibleContexts to be able to handle aliasing [Those kinds of types](http://stackoverflow.com/questions/27895196/haskell-illegal-polymorphic-type)
 - NoMonomorphismRestriction 