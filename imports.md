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

```haskell
-- aeson

Reexport {as = "js", mod = "Data.Aeson"}
Reexport {as = "js", mod = "Data.Aeson.Lens"}
Reexport {as = "js", mod = "Data.Aeson.Types"}
# js Data.Aeson.Types.Class not working because not exported, but I need the instances... !

-- containers

Reexport {as = "map", mod = "Data.Map.Strict"}
Reexport {as = "lmap", mod = "Data.Map"}
Reexport {as = "set", mod = "Data.Set"}
Reexport {as = "hm", mod = "Data.HashMap.Strict"}
Reexport {as = "ne", mod = "Data.List.NonEmpty"}
Reexport {as = "vec", mod = "Data.Vector"}

-- text

Reexport {as = "t", mod = "Data.Text"}
Reexport {as = "t", mod = "Data.Text.Encoding"}
Reexport {as = "t", mod = "Data.Text.IO"}
Reexport {as = "lt", mod = "Data.Text.Lazy"}
Reexport {as = "lt", mod = "Data.Text.Lazy.Encoding"}
Reexport {as = "lt", mod = "Data.Text.Lazy.IO"}

-- bytestring

Reexport {as = "bs", mod = "Data.ByteString"}
Reexport {as = "bs", mod = "Data.ByteString.Char8"}
Reexport {as = "lbs", mod = "Data.ByteString.Lazy"}
Reexport {as = "c8", mod = "Data.ByteString.Lazy.Char8"}

-- stm

Reexport {as = "stm", mod = "Control.Concurrent.STM"}

-- tar

Reexport {as = "tar", mod="Codec.Archive.Tar"}
Reexport {as = "tar", mod="Codec.Archive.Tar.Entry"}
Reexport {as = "tar", mod="Codec.Archive.Tar.Check"}

-- zlib

Reexport {as = "gzip", mod = "Codec.Compression.GZip"}

-- megaparsec

Reexport {as = "mp", mod = "Text.Megaparsec"}

-- ansi

Reexport {as = "ansi", mod = "System.Console.ANSI"}

-- optoparse-applicative

-- not opt Options.Applicative because it reexport too many things
-- | Parser type and low-level parsing functionality.
Reexport {as = "opt", mod = "Options.Applicative.Common"}
-- | Utilities to build parsers out of basic primitives.
Reexport {as = "opt", mod = "Options.Applicative.Builder"}
-- | Common completion functions.
Reexport {as = "opt", mod = "Options.Applicative.Builder.Completer"}
-- | Utilities to run parsers and display a help text.
Reexport {as = "opt", mod = "Options.Applicative.Extra"}


-- conduit
-- conduit-extra

Reexport {as = "c", mod = "Data.Conduit"}
Reexport {as = "cl", mod = "Data.Conduit.List"}
Reexport {as = "cb", mod = "Data.Conduit.Binary"}

-- lens

Reexport {as = "lens", mod = "Lens.Micro.Platform"}

-- misc

Reexport {as = "wai", mod = "Network.Wai"}
Reexport {as = "wai", mod = "Network.Wai.Handler.Warp"}
Reexport {as = "wai", mod = "Network.HTTP.Types.Status"}
Reexport {as = "wai", mod = "Network.Wai.Middleware.RequestLogger"}
Reexport {as = "wai", mod = "Network.Wai.Middleware.Static"}
Reexport {as = "ci", mod = "Data.CaseInsensitive"}

Reexport {as = "spock", mod = "Web.Spock.Digestive"}
Reexport {as = "spock", mod = "Web.Spock.Safe"}


Reexport {as = "trans", mod = "Control.Monad.Trans.Class"}
Reexport {as = "trans", mod = "Control.Monad.IO.Class"}
Reexport {as = "trans", mod = "Control.Monad.Trans.Class"}
Reexport {as = "trans", mod = "Control.Monad.Trans.State.Lazy"}

Reexport {as = "mtl", mod = "Control.Monad.State.Lazy"}

Reexport {as = "ws", mod = "Network.WebSockets"}
Reexport {as = "ws", mod = "Network.WebSockets.Connection"}

-- test purpose

Reexport {as = "demo", mod = "ReexportDemo"}
Reexport {as = "trans", mod = "Control.Monad.IO.Class"}

```