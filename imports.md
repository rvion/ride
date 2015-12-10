## Module definitions

:memo: this file is parsed by jetpack-gen.

 - All lines starting with  `  - ` will generate a qualified reexport
 - All lines starting with  `  * ` will generate a package dependency

:memo: one can look at modules listing on 
[stackage website](https://www.stackage.org/nightly-2015-12-10/docs)

:memo: I built the list of reexport below from experience and by looking
at github search result of popular projects such as `stack`.


### Packages:

  * text
  * bytestring
  * tar
  * zlib
  * megaparsec
  * conduit
  * conduit.extra

#### Containers

  - map Data.Map.Strict
  - set Data.Set
  - hm Data.HashMap.Strict
  - ne Data.List.NonEmpty

#### Text types 

_text_

  - t Data.Text
  - t Data.Text.Encoding
  - t Data.Text.IO
  - lt Data.Text.Lazy
  - lt Data.Text.Lazy.Encoding
  - lt Data.Text.Lazy.IO

_bytestring_

  - bs Data.ByteString
  - bs Data.ByteString.Char8
  - lbs Data.ByteString.Lazy
  - lbs Data.ByteString.Lazy.Char8

#### compression

_tar_

  - tar Codec.Archive.Tar
  - tar Codec.Archive.Tar.Entry
  - tar Codec.Archive.Tar.Check

_zlib_

  - gzip Codec.Compression.GZip

#### parser

_megaparsec_

  - mp Text.Megaparsec

#### conduit

_conduit_
_conduit-extra_

  - c Data.Conduit
  - cl Data.Conduit.List
  - cb Data.Conduit.Binary

