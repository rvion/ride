module Data.Text.Lazy.Encoding.AsLt
  ( module Data.Text.Lazy.Encoding.AsLt
  ) where
-- generated by https://github.com/rvion/ride/tree/master/jetpack-gen

import qualified Data.Text.Lazy.Encoding as I


-- lt_decodeLatin1 :: ByteString -> Text
lt_decodeLatin1 = I.decodeLatin1

-- lt_decodeUtf16BE :: ByteString -> Text
lt_decodeUtf16BE = I.decodeUtf16BE

-- lt_decodeUtf16BEWith :: OnDecodeError -> ByteString -> Text
lt_decodeUtf16BEWith = I.decodeUtf16BEWith

-- lt_decodeUtf16LE :: ByteString -> Text
lt_decodeUtf16LE = I.decodeUtf16LE

-- lt_decodeUtf16LEWith :: OnDecodeError -> ByteString -> Text
lt_decodeUtf16LEWith = I.decodeUtf16LEWith

-- lt_decodeUtf32BE :: ByteString -> Text
lt_decodeUtf32BE = I.decodeUtf32BE

-- lt_decodeUtf32BEWith :: OnDecodeError -> ByteString -> Text
lt_decodeUtf32BEWith = I.decodeUtf32BEWith

-- lt_decodeUtf32LE :: ByteString -> Text
lt_decodeUtf32LE = I.decodeUtf32LE

-- lt_decodeUtf32LEWith :: OnDecodeError -> ByteString -> Text
lt_decodeUtf32LEWith = I.decodeUtf32LEWith

-- lt_decodeUtf8 :: ByteString -> Text
lt_decodeUtf8 = I.decodeUtf8

-- lt_decodeUtf8' :: ByteString -> Either UnicodeException Text
lt_decodeUtf8' = I.decodeUtf8'

-- lt_decodeUtf8With :: OnDecodeError -> ByteString -> Text
lt_decodeUtf8With = I.decodeUtf8With

-- lt_encodeUtf16BE :: Text -> ByteString
lt_encodeUtf16BE = I.encodeUtf16BE

-- lt_encodeUtf16LE :: Text -> ByteString
lt_encodeUtf16LE = I.encodeUtf16LE

-- lt_encodeUtf32BE :: Text -> ByteString
lt_encodeUtf32BE = I.encodeUtf32BE

-- lt_encodeUtf32LE :: Text -> ByteString
lt_encodeUtf32LE = I.encodeUtf32LE

-- lt_encodeUtf8 :: Text -> ByteString
lt_encodeUtf8 = I.encodeUtf8

-- lt_encodeUtf8Builder :: Text -> Builder
lt_encodeUtf8Builder = I.encodeUtf8Builder

-- lt_encodeUtf8BuilderEscaped :: BoundedPrim Word8 -> Text -> Builder
lt_encodeUtf8BuilderEscaped = I.encodeUtf8BuilderEscaped
