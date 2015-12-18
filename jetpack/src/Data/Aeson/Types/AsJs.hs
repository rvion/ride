module Data.Aeson.Types.AsJs
  ( -- unqualified operators re-export
  (I..!=), (I..:), (I..:?), (I..=)
  , module Data.Aeson.Types.AsJs
  ) where
-- generated by https://github.com/rvion/ride/tree/master/jetpack-gen

import qualified Data.Aeson.Types as I


-- js_typeMismatch :: forall a. String -> Value -> Parser a
js_typeMismatch = I.typeMismatch

-- js_camelTo :: Char -> String -> String
js_camelTo = I.camelTo

-- js_defaultOptions :: Options
js_defaultOptions = I.defaultOptions

-- js_defaultTaggedObject :: SumEncoding
js_defaultTaggedObject = I.defaultTaggedObject

-- js_emptyArray :: Value
js_emptyArray = I.emptyArray

-- js_emptyObject :: Value
js_emptyObject = I.emptyObject

-- js_modifyFailure :: forall a. (String -> String) -> Parser a -> Parser a
js_modifyFailure = I.modifyFailure

-- js_parse :: forall a b. (a -> Parser b) -> a -> Result b
js_parse = I.parse

-- js_parseEither :: forall a b. (a -> Parser b) -> a -> Either String b
js_parseEither = I.parseEither

-- js_parseMaybe :: forall a b. (a -> Parser b) -> a -> Maybe b
js_parseMaybe = I.parseMaybe

type JsOptions  = I.Options
get_js_fieldLabelModifier o = I.fieldLabelModifier o
set_js_fieldLabelModifier x o = o { I.fieldLabelModifier = x}
get_js_constructorTagModifier o = I.constructorTagModifier o
set_js_constructorTagModifier x o = o { I.constructorTagModifier = x}
get_js_allNullaryToStringTag o = I.allNullaryToStringTag o
set_js_allNullaryToStringTag x o = o { I.allNullaryToStringTag = x}
get_js_omitNothingFields o = I.omitNothingFields o
set_js_omitNothingFields x o = o { I.omitNothingFields = x}
get_js_sumEncoding o = I.sumEncoding o
set_js_sumEncoding x o = o { I.sumEncoding = x}

-- constructor :: String -> String -> String -> String -> Bool -> Bool -> SumEncoding -> Options
js_mk'Options =  I.Options
pattern JsOptions a b c d e <-  I.Options a b c d e

type JsPair  = I.Pair

type JsParser a = I.Parser a

type JsSumEncoding  = I.SumEncoding
get_js_tagFieldName o = I.tagFieldName o
set_js_tagFieldName x o = o { I.tagFieldName = x}
get_js_contentsFieldName o = I.contentsFieldName o
set_js_contentsFieldName x o = o { I.contentsFieldName = x}

-- constructor :: String -> String -> TaggedObject
js_mk'TaggedObject =  I.TaggedObject
pattern JsTaggedObject a b <-  I.TaggedObject a b

-- constructor :: ObjectWithSingleField
js_mk'ObjectWithSingleField =  I.ObjectWithSingleField
pattern JsObjectWithSingleField  <-  I.ObjectWithSingleField 

-- constructor :: TwoElemArray
js_mk'TwoElemArray =  I.TwoElemArray
pattern JsTwoElemArray  <-  I.TwoElemArray 
