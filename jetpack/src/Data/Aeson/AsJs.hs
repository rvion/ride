module Data.Aeson.AsJs
  ( -- unqualified class re-export
  I.FromJSON(I.parseJSON), I.GFromJSON(I.gParseJSON), I.GToJSON(I.gToJSON), I.ToJSON(I.toJSON)
  , module Data.Aeson.AsJs
  ) where
-- generated by https://github.com/rvion/ride/tree/master/jetpack-gen

import qualified Data.Aeson as I


-- js_decode :: forall a. FromJSON a => ByteString -> Maybe a
js_decode = I.decode

-- js_decode' :: forall a. FromJSON a => ByteString -> Maybe a
js_decode' = I.decode'

-- js_decodeStrict :: forall a. FromJSON a => ByteString -> Maybe a
js_decodeStrict = I.decodeStrict

-- js_decodeStrict' :: forall a. FromJSON a => ByteString -> Maybe a
js_decodeStrict' = I.decodeStrict'

-- js_eitherDecode :: forall a. FromJSON a => ByteString -> Either String a
js_eitherDecode = I.eitherDecode

-- js_eitherDecode' :: forall a. FromJSON a => ByteString -> Either String a
js_eitherDecode' = I.eitherDecode'

-- js_eitherDecodeStrict :: forall a. FromJSON a => ByteString -> Either String a
js_eitherDecodeStrict = I.eitherDecodeStrict

-- js_eitherDecodeStrict' :: forall a. FromJSON a => ByteString -> Either String a
js_eitherDecodeStrict' = I.eitherDecodeStrict'

-- js_encode :: forall a. ToJSON a => a -> ByteString
js_encode = I.encode

-- js_json :: Parser Value
js_json = I.json

-- js_json' :: Parser Value
js_json' = I.json'

-- js_genericParseJSON :: forall a. (Generic a, GFromJSON (Rep a)) => Options -> Value -> Parser a
js_genericParseJSON = I.genericParseJSON

-- js_genericToJSON :: forall a. (Generic a, GToJSON (Rep a)) => Options -> a -> Value
js_genericToJSON = I.genericToJSON

-- (.!=) :: forall a. Parser (Maybe a) -> a -> Parser a
(.!=) = (I..!=)

-- (.:) :: forall a. FromJSON a => Object -> Text -> Parser a
(.:) = (I..:)

-- (.:?) :: forall a. FromJSON a => Object -> Text -> Parser (Maybe a)
(.:?) = (I..:?)

-- (.=) :: forall a. ToJSON a => Text -> a -> Pair
(.=) = (I..=)

-- js_fromJSON :: forall a. FromJSON a => Value -> Result a
js_fromJSON = I.fromJSON

-- js_withArray :: forall a. String -> (Array -> Parser a) -> Value -> Parser a
js_withArray = I.withArray

-- js_withBool :: forall a. String -> (Bool -> Parser a) -> Value -> Parser a
js_withBool = I.withBool

-- js_withNumber :: forall a. String -> (Number -> Parser a) -> Value -> Parser a
js_withNumber = I.withNumber

-- js_withObject :: forall a. String -> (Object -> Parser a) -> Value -> Parser a
js_withObject = I.withObject

-- js_withScientific :: forall a. String -> (Scientific -> Parser a) -> Value -> Parser a
js_withScientific = I.withScientific

-- js_withText :: forall a. String -> (Text -> Parser a) -> Value -> Parser a
js_withText = I.withText

-- js_object :: [Pair] -> Value
js_object = I.object

type JsArray  = I.Array

type JsDotNetTime  = I.DotNetTime

type JsObject  = I.Object

type JsResult a = I.Result a

-- constructor :: String -> Error
js_mk'Error =  I.Error
pattern JsError a <-  I.Error a

-- constructor :: a -> Success
js_mk'Success =  I.Success
pattern JsSuccess a <-  I.Success a

type JsValue  = I.Value

-- constructor :: Object -> Object
js_mk'Object =  I.Object
pattern JsObject a <-  I.Object a

-- constructor :: Array -> Array
js_mk'Array =  I.Array
pattern JsArray a <-  I.Array a

-- constructor :: Text -> String
js_mk'String =  I.String
pattern JsString a <-  I.String a

-- constructor :: Scientific -> Number
js_mk'Number =  I.Number
pattern JsNumber a <-  I.Number a

-- constructor :: Bool -> Bool
js_mk'Bool =  I.Bool
pattern JsBool a <-  I.Bool a

-- constructor :: Null
js_mk'Null =  I.Null
pattern JsNull  <-  I.Null 
