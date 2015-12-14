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
  * transformers
  * semigroups
  * websockets
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
  * wai
  * warp
  * http-types
  * case-insensitive
  * Spock
  * Spock-digestive
  * aeson
  * lens-aeson
  * mtl
  * wai-extra
  * wai-middleware-static
  * vector

### Modules

_aeson_

  - js Data.Aeson
  - js Data.Aeson.Lens

_containers_

  - map Data.Map.Strict
  - lmap Data.Map
  - set Data.Set
  - hm Data.HashMap.Strict
  - ne Data.List.NonEmpty
  - vec Data.Vector

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
  - c8 Data.ByteString.Lazy.Char8

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

  - lens Lens.Micro.Platform

_test purpose:_
  - demo ReexportDemo


  - wai Network.Wai
  - wai Network.Wai.Handler.Warp
  - wai Network.HTTP.Types.Status
  - wai Network.Wai.Middleware.RequestLogger
  - wai Network.Wai.Middleware.Static
  - ci Data.CaseInsensitive

  - spock Web.Spock.Digestive
  - spock Web.Spock.Safe


  - trans Control.Monad.Trans.Class
  - trans Control.Monad.IO.Class
  - trans Control.Monad.Trans.Class
  - trans Control.Monad.Trans.State.Lazy

  - mtl Control.Monad.State.Lazy


  - ws Network.WebSockets
  - ws Network.WebSockets.Connection
