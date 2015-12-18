## Module definitions

:memo:

  1. this file is parsed by jetpack-gen.
  2. one can look at modules listing on
      [stackage website](https://www.stackage.org/nightly-2015-12-10/docs)
  3. I built the list of reexport below from experience and by looking
     at github search result of popular projects such as `stack`.

### Modules

### test purpose

-- ReexportUnqualified {mod = "ReexportDemo"}

```haskell
-- Qualified {as = "demo", mod = "ReexportDemo"}
```

### Containers

```haskell
Qualified {as = "map", mod = "Data.Map.Strict"}
Qualified {as = "lmap", mod = "Data.Map"}
Qualified {as = "set", mod = "Data.Set"}
Qualified {as = "ci", mod = "Data.CaseInsensitive"}
Qualified {as = "hm", mod = "Data.HashMap.Strict"}
Qualified {as = "ne", mod = "Data.List.NonEmpty"}
Qualified {as = "vec", mod = "Data.Vector"}
```

### Text manipulation (text, bytestring)

```haskell
Qualified {as = "t", mod = "Data.Text"}
Qualified {as = "t", mod = "Data.Text.Encoding"}
Qualified {as = "t", mod = "Data.Text.IO"}
Qualified {as = "lt", mod = "Data.Text.Lazy"}
Qualified {as = "lt", mod = "Data.Text.Lazy.Encoding"}
Qualified {as = "lt", mod = "Data.Text.Lazy.IO"}
Qualified {as = "bs", mod = "Data.ByteString"}
Qualified {as = "bs", mod = "Data.ByteString.Char8"}
Qualified {as = "lbs", mod = "Data.ByteString.Lazy"}
Qualified {as = "c8", mod = "Data.ByteString.Lazy.Char8"}
```

### Software Architecture (stm, conduit)

```haskell
Qualified {as = "ctrl", mod = "Control.Concurrent"}
Qualified {as = "stm", mod = "Control.Concurrent.STM"}
Qualified {as = "trans", mod = "Control.Monad.IO.Class"}
Qualified {as = "trans", mod = "Control.Monad.Trans.Resource"}
Qualified {as = "trans", mod = "Control.Monad.Trans.Class"}
Qualified {as = "trans", mod = "Control.Monad.Trans.State.Lazy"}
-- Qualified {as = "mtl", mod = "Control.Monad.State.Lazy"}
```

### Streaming

```haskell
Qualified {as = "c", mod = "Data.Conduit"}
Qualified {as = "cl", mod = "Data.Conduit.List"}
Qualified {as = "cb", mod = "Data.Conduit.Binary"}
```

### Parsing (megaparsec)

```haskell
Qualified {as = "parse", mod = "Text.Megaparsec"}
```

### compression

```haskell
Qualified {as = "tar", mod="Codec.Archive.Tar"}
Qualified {as = "tar", mod="Codec.Archive.Tar.Entry"}
Qualified {as = "tar", mod="Codec.Archive.Tar.Check"}
Qualified {as = "gzip", mod = "Codec.Compression.GZip"}
```

### Cmd-line tools: (ansi colors, optoparse-applicative)

  - Options.Applicative.Common:            Parser type and low-level parsing functionality.
  - Options.Applicative.Builder:           Utilities to build parsers out of basic primitives.
  - Options.Applicative.Builder.Completer: Common completion functions.
  - Options.Applicative.Extra:             Utilities to run parsers and display a help text.

```haskell
Qualified {as = "ansi", mod = "System.Console.ANSI"}
Qualified {as = "sio", mod = "System.IO.Strict"}

Qualified {as = "env", mod = "System.IO"}
Qualified {as = "env", mod = "System.Directory"}
Qualified {as = "env", mod = "System.Environment"}
Qualified {as = "env", mod = "System.Exit"}
Qualified {as = "env", mod = "System.Process"}
Qualified {as = "opt", mod = "Options.Applicative.Common"}
Qualified {as = "opt", mod = "Options.Applicative.Builder"}
Qualified {as = "opt", mod = "Options.Applicative.Builder.Completer"}
Qualified {as = "opt", mod = "Options.Applicative.Extra"}
```

### Web: (wai, aeson, spock)

```haskell
Qualified {as = "wai", mod = "Network.Wai"}
Qualified {as = "wai", mod = "Network.Wai.Handler.Warp"}
Qualified {as = "wai", mod = "Network.Wai.Middleware.RequestLogger"}
Qualified {as = "wai", mod = "Network.Wai.Middleware.Static"}

Qualified {as = "http", mod = "Network.HTTP.Client"}
Qualified {as = "http", mod = "Network.HTTP.Types"}
Qualified {as = "http", mod = "Network.HTTP.Types.Status"}
Qualified {as = "http", mod = "Network.HTTP.Types.Header"}
Qualified {as = "http", mod = "Network.HTTP.Conduit"}

Qualified {as = "spock", mod = "Web.Spock.Digestive"}
Qualified {as = "spock", mod = "Web.Spock.Safe"}

Qualified {as = "ws", mod = "Network.WebSockets"}
Qualified {as = "ws", mod = "Network.WebSockets.Connection"}

Qualified {as = "js", mod = "Data.Aeson"}
Qualified {as = "js", mod = "Data.Aeson.Types"}
Qualified {as = "js", mod = "Data.Aeson.Lens"}

Qualified {as = "uri", mod = "Network.URI"}
```
# js Data.Aeson.Types.Class not working because not exported, but I need the instances... !

### Lens (lens)

```haskell
Qualified {as = "lens", mod = "Lens.Micro.Platform"}
```

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
  * process
  * http-conduit
  * network-uri
  * directory
  * strict
  * resourcet
