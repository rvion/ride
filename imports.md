## Module definitions

:memo:

  1. this file is parsed by jetpack-gen.
      - All lines starting with  `  - ` will generate a qualified reexport
      - All lines starting with  `  * ` will generate a package dependency

  2. one can look at modules listing on 
      [stackage website](https://www.stackage.org/nightly-2015-12-10/docs)

  3. I built the list of reexport below from experience and by looking
     at github search result of popular projects such as `stack`.


### Packages

  * base
  * base-prelude
  * unordered-containers
  * containers
  * semigroups
  * text
  * bytestring
  * tar
  * zlib
  * megaparsec
  * conduit
  * conduit-extra
  * microlens-platform
  * reexport-demo

a ### Modules
a 
a   - map_lazy Data.Map
a   - map Data.Map.Strict
a   - set Data.Set
a   - hm Data.HashMap.Strict
a   - ne Data.List.NonEmpty
a 
a _text_
a 
a   - t Data.Text
a   - t Data.Text.Encoding
a   - t Data.Text.IO
a   - lt Data.Text.Lazy
a   - lt Data.Text.Lazy.Encoding
a   - lt Data.Text.Lazy.IO
a 
a _bytestring_
a 
a   - bs Data.ByteString
a   - bs Data.ByteString.Char8
a   - lbs Data.ByteString.Lazy
a   - lbs Data.ByteString.Lazy.Char8
a 
a _tar_
a 
a   - tar Codec.Archive.Tar
a   - tar Codec.Archive.Tar.Entry
a   - tar Codec.Archive.Tar.Check
a 
a _zlib_
a 
a   - gzip Codec.Compression.GZip
a 
a _megaparsec_
a 
a   - mp Text.Megaparsec
a 
a _conduit_
a _conduit-extra_
a 
a   - c Data.Conduit
a   - cl Data.Conduit.List
a   - cb Data.Conduit.Binary
a 
a _lens_
a 
a   - lens Lens.Micro.Platform

_test purpose:_

  - demo ReexportDemo
