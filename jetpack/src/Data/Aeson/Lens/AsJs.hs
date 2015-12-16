module Data.Aeson.Lens.AsJs
  ( -- unqualified class re-export
  I.AsJSON(I._JSON), I.AsNumber(I._Number, I._Double, I._Integer), I.AsPrimitive(I._Primitive, I._String, I._Bool, I._Null), I.AsValue(I._Value, I._Object, I._Array)
  , module Data.Aeson.Lens.AsJs
  ) where
-- generated by https://github.com/rvion/ride/tree/master/jetpack-gen

import qualified Data.Aeson.Lens as I


-- js__Integral :: forall t a. (AsNumber t, Integral a) => Prism' t a
js__Integral = I._Integral

-- js_key :: forall t. AsValue t => Text -> Traversal' t Value
js_key = I.key

-- js_members :: forall t. AsValue t => IndexedTraversal' Text t Value
js_members = I.members

-- js_nonNull :: Prism' Value Value
js_nonNull = I.nonNull

-- js_nth :: forall t. AsValue t => Int -> Traversal' t Value
js_nth = I.nth

-- js_values :: forall t. AsValue t => IndexedTraversal' Int t Value
js_values = I.values

type JsPrimitive  = I.Primitive

-- constructor :: Text -> StringPrim
js_mk'StringPrim =  I.StringPrim
pattern JsStringPrim a <-  I.StringPrim a

-- constructor :: Scientific -> NumberPrim
js_mk'NumberPrim =  I.NumberPrim
pattern JsNumberPrim a <-  I.NumberPrim a

-- constructor :: Bool -> BoolPrim
js_mk'BoolPrim =  I.BoolPrim
pattern JsBoolPrim a <-  I.BoolPrim a

-- constructor :: NullPrim
js_mk'NullPrim =  I.NullPrim
pattern JsNullPrim  <-  I.NullPrim 
