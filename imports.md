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
  * stm
  * ansi-terminal
  * optparse-applicative

### Modules

_containers_

  - m Data.Map.Strict
  - lm Data.Map
  - set Data.Set
  - hm Data.HashMap.Strict
  - ne Data.List.NonEmpty

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

_stm_

  - stm Control.Concurrent.STM

_tar_

  - tar Codec.Archive.Tar
  - tar Codec.Archive.Tar.Entry
  - tar Codec.Archive.Tar.Check

_zlib_

  - gzip Codec.Compression.GZip

_megaparsec_

  - mp Text.Megaparsec

_ansi_

  - ansi System.Console.ANSI

_optoparse-applicative_

  - opt Options.Applicative

_conduit_
_conduit-extra_

  - c Data.Conduit
  - cl Data.Conduit.List
  - cb Data.Conduit.Binary

_lens_

-- http://stackoverflow.com/questions/27895196/haskell-illegal-polymorphic-type makes it hard
  - lens Lens.Micro.Platform

_test purpose:_
  - demo ReexportDemo

