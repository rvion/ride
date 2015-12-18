module Codec.Compression.GZip.AsGzip
  ( module Codec.Compression.GZip.AsGzip
  ) where
-- generated by https://github.com/rvion/ride/tree/master/jetpack-gen

import qualified Codec.Compression.GZip as I


-- gzip_compress :: ByteString -> ByteString
gzip_compress = I.compress

-- gzip_compressWith :: CompressParams -> ByteString -> ByteString
gzip_compressWith = I.compressWith

-- gzip_decompress :: ByteString -> ByteString
gzip_decompress = I.decompress

-- gzip_decompressWith :: DecompressParams -> ByteString -> ByteString
gzip_decompressWith = I.decompressWith

-- gzip_defaultCompressParams :: CompressParams
gzip_defaultCompressParams = I.defaultCompressParams

-- gzip_defaultDecompressParams :: DecompressParams
gzip_defaultDecompressParams = I.defaultDecompressParams

-- gzip_bestCompression :: CompressionLevel
gzip_bestCompression = I.bestCompression

-- gzip_bestSpeed :: CompressionLevel
gzip_bestSpeed = I.bestSpeed

-- gzip_compressionLevel :: Int -> CompressionLevel
gzip_compressionLevel = I.compressionLevel

-- gzip_defaultCompression :: CompressionLevel
gzip_defaultCompression = I.defaultCompression

-- gzip_defaultMemoryLevel :: MemoryLevel
gzip_defaultMemoryLevel = I.defaultMemoryLevel

-- gzip_defaultStrategy :: CompressionStrategy
gzip_defaultStrategy = I.defaultStrategy

-- gzip_defaultWindowBits :: WindowBits
gzip_defaultWindowBits = I.defaultWindowBits

-- gzip_deflateMethod :: Method
gzip_deflateMethod = I.deflateMethod

-- gzip_filteredStrategy :: CompressionStrategy
gzip_filteredStrategy = I.filteredStrategy

-- gzip_huffmanOnlyStrategy :: CompressionStrategy
gzip_huffmanOnlyStrategy = I.huffmanOnlyStrategy

-- gzip_maxMemoryLevel :: MemoryLevel
gzip_maxMemoryLevel = I.maxMemoryLevel

-- gzip_memoryLevel :: Int -> MemoryLevel
gzip_memoryLevel = I.memoryLevel

-- gzip_minMemoryLevel :: MemoryLevel
gzip_minMemoryLevel = I.minMemoryLevel

-- gzip_noCompression :: CompressionLevel
gzip_noCompression = I.noCompression

-- gzip_windowBits :: Int -> WindowBits
gzip_windowBits = I.windowBits

type GzipCompressParams  = I.CompressParams
get_gzip_compressLevel o = I.compressLevel o
set_gzip_compressLevel x o = o { I.compressLevel = x}
get_gzip_compressMethod o = I.compressMethod o
set_gzip_compressMethod x o = o { I.compressMethod = x}
get_gzip_compressWindowBits o = I.compressWindowBits o
set_gzip_compressWindowBits x o = o { I.compressWindowBits = x}
get_gzip_compressMemoryLevel o = I.compressMemoryLevel o
set_gzip_compressMemoryLevel x o = o { I.compressMemoryLevel = x}
get_gzip_compressStrategy o = I.compressStrategy o
set_gzip_compressStrategy x o = o { I.compressStrategy = x}
get_gzip_compressBufferSize o = I.compressBufferSize o
set_gzip_compressBufferSize x o = o { I.compressBufferSize = x}
get_gzip_compressDictionary o = I.compressDictionary o
set_gzip_compressDictionary x o = o { I.compressDictionary = x}

-- constructor :: CompressionLevel -> Method -> WindowBits -> MemoryLevel -> CompressionStrategy -> Int -> Maybe ByteString -> CompressParams
gzip_mk'CompressParams =  I.CompressParams
pattern GzipCompressParams a b c d e f g <-  I.CompressParams a b c d e f g

type GzipDecompressParams  = I.DecompressParams
get_gzip_decompressWindowBits o = I.decompressWindowBits o
set_gzip_decompressWindowBits x o = o { I.decompressWindowBits = x}
get_gzip_decompressBufferSize o = I.decompressBufferSize o
set_gzip_decompressBufferSize x o = o { I.decompressBufferSize = x}
get_gzip_decompressDictionary o = I.decompressDictionary o
set_gzip_decompressDictionary x o = o { I.decompressDictionary = x}

-- constructor :: WindowBits -> Int -> Maybe ByteString -> DecompressParams
gzip_mk'DecompressParams =  I.DecompressParams
pattern GzipDecompressParams a b c <-  I.DecompressParams a b c

type GzipCompressionLevel  = I.CompressionLevel

-- constructor :: DefaultCompression
gzip_mk'DefaultCompression =  I.DefaultCompression
pattern GzipDefaultCompression  <-  I.DefaultCompression

-- constructor :: NoCompression
gzip_mk'NoCompression =  I.NoCompression
pattern GzipNoCompression  <-  I.NoCompression

-- constructor :: BestSpeed
gzip_mk'BestSpeed =  I.BestSpeed
pattern GzipBestSpeed  <-  I.BestSpeed

-- constructor :: BestCompression
gzip_mk'BestCompression =  I.BestCompression
pattern GzipBestCompression  <-  I.BestCompression

-- constructor :: Int -> CompressionLevel
gzip_mk'CompressionLevel =  I.CompressionLevel
pattern GzipCompressionLevel a <-  I.CompressionLevel a

type GzipCompressionStrategy  = I.CompressionStrategy

-- constructor :: DefaultStrategy
gzip_mk'DefaultStrategy =  I.DefaultStrategy
pattern GzipDefaultStrategy  <-  I.DefaultStrategy

-- constructor :: Filtered
gzip_mk'Filtered =  I.Filtered
pattern GzipFiltered  <-  I.Filtered

-- constructor :: HuffmanOnly
gzip_mk'HuffmanOnly =  I.HuffmanOnly
pattern GzipHuffmanOnly  <-  I.HuffmanOnly

type GzipMemoryLevel  = I.MemoryLevel

-- constructor :: DefaultMemoryLevel
gzip_mk'DefaultMemoryLevel =  I.DefaultMemoryLevel
pattern GzipDefaultMemoryLevel  <-  I.DefaultMemoryLevel

-- constructor :: MinMemoryLevel
gzip_mk'MinMemoryLevel =  I.MinMemoryLevel
pattern GzipMinMemoryLevel  <-  I.MinMemoryLevel

-- constructor :: MaxMemoryLevel
gzip_mk'MaxMemoryLevel =  I.MaxMemoryLevel
pattern GzipMaxMemoryLevel  <-  I.MaxMemoryLevel

-- constructor :: Int -> MemoryLevel
gzip_mk'MemoryLevel =  I.MemoryLevel
pattern GzipMemoryLevel a <-  I.MemoryLevel a

type GzipMethod  = I.Method

-- constructor :: Deflated
gzip_mk'Deflated =  I.Deflated
pattern GzipDeflated  <-  I.Deflated

type GzipWindowBits  = I.WindowBits

-- constructor :: Int -> WindowBits
gzip_mk'WindowBits =  I.WindowBits
pattern GzipWindowBits a <-  I.WindowBits a

-- constructor :: DefaultWindowBits
gzip_mk'DefaultWindowBits =  I.DefaultWindowBits
pattern GzipDefaultWindowBits  <-  I.DefaultWindowBits
