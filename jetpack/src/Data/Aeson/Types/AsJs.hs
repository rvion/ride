module Data.Aeson.Types.AsJs where
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
js_mk'Options =  I.Options-- constructor
pattern JsOptions a b c d e <-  I.Options a b c d e
type JsPair  = I.Pair
type JsParser a = I.Parser a
type JsSumEncoding  = I.SumEncoding
js_mk'TaggedObject =  I.TaggedObject-- constructor
pattern JsTaggedObject a b <-  I.TaggedObject a b
js_mk'ObjectWithSingleField =  I.ObjectWithSingleField-- constructor
pattern JsObjectWithSingleField  <-  I.ObjectWithSingleField 
js_mk'TwoElemArray =  I.TwoElemArray-- constructor
pattern JsTwoElemArray  <-  I.TwoElemArray 
