## Module definitions

:memo: this file is parsed by jetpack-gen. All lines starting with  `-` will
generate a qualified reexport

:memo: one can look at modules listing on 
[stackage website](https://www.stackage.org/nightly-2015-12-10/docs)

:memo: I built the list of reexport below from experience and by looking
at github search result of popular projects such as `stack`.

#### Containers

  - map Data.Map.Strict
  - set Data.Set
  - hm Data.HashMap.Strict
  - ne Data.List.NonEmpty

#### Text types 

  - t Data.Text.Strict
  - t Data.Text.Encoding
  - t Data.Text.IO

  - lt Data.Text.Lazy.Strict
  - lt Data.Text.Lazy.Encoding
  - lt Data.Text.Lazy.IO

  - bs Data.ByteString
  - bs Data.ByteString.Char8

  - lbs Data.ByteString.Lazy
  - lbs Data.ByteString.Lazy.Char8

#### compression

  - tar Codec.Archive.Tar
  - tar Codec.Archive.Tar.Entry
  - tar Codec.Archive.Tar.Check

  - gzip Codec.Compression.GZip

#### parser

  - mp Text.Megaparsec

#### conduit

  - c Data.Conduit
  - cl Data.Conduit.List
  - cb Data.Conduit.Binary