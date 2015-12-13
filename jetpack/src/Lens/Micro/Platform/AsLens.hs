module Lens.Micro.Platform.AsLens where
-- generated by rvion/jetpack-gen 

import Lens.Micro.Platform as I

-- (&) :: forall a b. a -> (a -> b) -> b
(&) = (I.&)

-- (%=) :: forall s a b (m :: * -> *). MonadState s m => ASetter s s a b -> (a -> b) -> m ()
(%=) = (I.%=)

-- (*=) :: forall s a (m :: * -> *). (MonadState s m, Num a) => ASetter s s a a -> a -> m ()
(*=) = (I.*=)

-- (+=) :: forall s a (m :: * -> *). (MonadState s m, Num a) => ASetter s s a a -> a -> m ()
(+=) = (I.+=)

-- (-=) :: forall s a (m :: * -> *). (MonadState s m, Num a) => ASetter s s a a -> a -> m ()
(-=) = (I.-=)

-- (.=) :: forall s a b (m :: * -> *). MonadState s m => ASetter s s a b -> b -> m ()
(.=) = (I..=)

-- (//=) :: forall s a (m :: * -> *). (MonadState s m, Fractional a) => ASetter s s a a -> a -> m ()
(//=) = (I.//=)

-- lens_preview :: forall a s (m :: * -> *). MonadReader s m => Getting (First a) s a -> m (Maybe a)
lens_preview = I.preview

-- lens_use :: forall a s (m :: * -> *). MonadState s m => Getting a s a -> m a
lens_use = I.use

-- lens_view :: forall a s (m :: * -> *). MonadReader s m => Getting a s a -> m a
lens_view = I.view

-- lens_camelCaseFields :: LensRules
lens_camelCaseFields = I.camelCaseFields

-- lens_generateLazyPatterns :: Lens' LensRules Bool
lens_generateLazyPatterns = I.generateLazyPatterns

-- lens_generateSignatures :: Lens' LensRules Bool
lens_generateSignatures = I.generateSignatures

-- lens_generateUpdateableOptics :: Lens' LensRules Bool
lens_generateUpdateableOptics = I.generateUpdateableOptics

-- lens_lensField :: Lens' LensRules (Name -> [Name] -> Name -> [DefName])
lens_lensField = I.lensField

-- lens_lensRules :: LensRules
lens_lensRules = I.lensRules

-- lens_lensRulesFor :: [(String, String)] -> LensRules
lens_lensRulesFor = I.lensRulesFor

-- lens_makeFields :: Name -> DecsQ
lens_makeFields = I.makeFields

-- lens_makeLenses :: Name -> DecsQ
lens_makeLenses = I.makeLenses

-- lens_makeLensesFor :: [(String, String)] -> Name -> DecsQ
lens_makeLensesFor = I.makeLensesFor

-- lens_makeLensesWith :: LensRules -> Name -> DecsQ
lens_makeLensesWith = I.makeLensesWith

-- lens_simpleLenses :: Lens' LensRules Bool
lens_simpleLenses = I.simpleLenses

-- (%~) :: forall s t a b. ASetter s t a b -> (a -> b) -> s -> t
(%~) = (I.%~)

-- (.~) :: forall s t a b. ASetter s t a b -> b -> s -> t
(.~) = (I..~)

-- (<%~) :: forall b s t a. LensLike ((,) b) s t a b -> (a -> b) -> s -> (b, t)
(<%~) = (I.<%~)

-- (<<%~) :: forall a s t b. LensLike ((,) a) s t a b -> (a -> b) -> s -> (a, t)
(<<%~) = (I.<<%~)

-- (<<.~) :: forall a s t b. LensLike ((,) a) s t a b -> b -> s -> (a, t)
(<<.~) = (I.<<.~)

-- (^.) :: forall s a. s -> Getting a s a -> a
(^.) = (I.^.)

-- (^..) :: forall s a. s -> Getting (Endo [a]) s a -> [a]
(^..) = (I.^..)

-- (^?) :: forall s a. s -> Getting (First a) s a -> Maybe a
(^?) = (I.^?)

-- (^?!) :: forall s a. s -> Getting (Endo a) s a -> a
(^?!) = (I.^?!)

-- lens__Just :: forall a a'. Traversal (Maybe a) (Maybe a') a a'
lens__Just = I._Just

-- lens__Left :: forall a b a'. Traversal (Either a b) (Either a' b) a a'
lens__Left = I._Left

-- lens__Nothing :: forall a. Traversal' (Maybe a) ()
lens__Nothing = I._Nothing

-- lens__Right :: forall a b b'. Traversal (Either a b) (Either a b') b b'
lens__Right = I._Right

-- lens__head :: forall s a. Cons s s a a => Traversal' s a
lens__head = I._head

-- lens__init :: forall s a. Snoc s s a a => Traversal' s s
lens__init = I._init

-- lens__last :: forall s a. Snoc s s a a => Traversal' s a
lens__last = I._last

-- lens__tail :: forall s a. Cons s s a a => Traversal' s s
lens__tail = I._tail

-- lens_both :: forall a b. Traversal (a, a) (b, b) a b
lens_both = I.both

-- lens_failing :: forall s t a b. Traversal s t a b -> Traversal s t a b -> Traversal s t a b
lens_failing = I.failing

-- lens_filtered :: forall a. (a -> Bool) -> Traversal' a a
lens_filtered = I.filtered

-- lens_has :: forall s a. Getting Any s a -> s -> Bool
lens_has = I.has

-- lens_lens :: forall s a b t. (s -> a) -> (s -> b -> t) -> Lens s t a b
lens_lens = I.lens

-- lens_mapped :: forall (f :: * -> *) a b. Functor f => ASetter (f a) (f b) a b
lens_mapped = I.mapped

-- lens_non :: forall a. Eq a => a -> Lens' (Maybe a) a
lens_non = I.non

-- lens_over :: forall s t a b. ASetter s t a b -> (a -> b) -> s -> t
lens_over = I.over

-- lens_set :: forall s t a b. ASetter s t a b -> b -> s -> t
lens_set = I.set

-- lens_to :: forall s a r. (s -> a) -> Getting r s a
lens_to = I.to

-- lens_toListOf :: forall a s. Getting (Endo [a]) s a -> s -> [a]
lens_toListOf = I.toListOf

-- lens_folded :: forall r (f :: * -> *) a. (Foldable f, Applicative (Const r)) => Getting r (f a) a
lens_folded = I.folded

-- lens_sets :: forall a b s t. ((a -> b) -> s -> t) -> ASetter s t a b
lens_sets = I.sets

-- lens_traversed :: forall (f :: * -> *) a b. Traversable f => Traversal (f a) (f b) a b
lens_traversed = I.traversed

type LensDefName  = I.DefName 
type LensFold a b = I.Fold a b
type LensGetter a b = I.Getter a b
type LensLensRules  = I.LensRules 
type LensASetter a b c d = I.ASetter a b c d
type LensASetter' a b = I.ASetter' a b
type LensGetting a b c = I.Getting a b c
type LensLens a b c d = I.Lens a b c d
type LensLens' a b = I.Lens' a b
type LensLensLike a b c d e = I.LensLike a b c d e
type LensLensLike' a b c = I.LensLike' a b c
type LensTraversal a b c d = I.Traversal a b c d
type LensTraversal' a b = I.Traversal' a b
