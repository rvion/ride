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
-- Reexport {as = "demo", mod = "ReexportDemo"}
```

### Containers

```haskell
Reexport {as = "map", mod = "Data.Map.Strict"}
Reexport {as = "lmap", mod = "Data.Map"}
Reexport {as = "set", mod = "Data.Set"}
Reexport {as = "ci", mod = "Data.CaseInsensitive"}
Reexport {as = "hm", mod = "Data.HashMap.Strict"}
Reexport {as = "ne", mod = "Data.List.NonEmpty"}
Reexport {as = "vec", mod = "Data.Vector"}
```

### Text manipulation (text, bytestring)

```haskell
Reexport {as = "t", mod = "Data.Text"}
Reexport {as = "t", mod = "Data.Text.Encoding"}
Reexport {as = "t", mod = "Data.Text.IO"}
Reexport {as = "lt", mod = "Data.Text.Lazy"}
Reexport {as = "lt", mod = "Data.Text.Lazy.Encoding"}
Reexport {as = "lt", mod = "Data.Text.Lazy.IO"}
Reexport {as = "bs", mod = "Data.ByteString"}
Reexport {as = "bs", mod = "Data.ByteString.Char8"}
Reexport {as = "lbs", mod = "Data.ByteString.Lazy"}
Reexport {as = "c8", mod = "Data.ByteString.Lazy.Char8"}
```

### Software Architecture (stm, conduit)

```haskell
Reexport {as = "ctrl", mod = "Control.Concurrent"}
Reexport {as = "stm", mod = "Control.Concurrent.STM"}
Reexport {as = "trans", mod = "Control.Monad.IO.Class"}
Reexport {as = "trans", mod = "Control.Monad.Trans.Class"}
Reexport {as = "trans", mod = "Control.Monad.Trans.State.Lazy"}
Reexport {as = "mtl", mod = "Control.Monad.State.Lazy"}
```

### Streaming

```haskell
Reexport {as = "c", mod = "Data.Conduit"}
Reexport {as = "cl", mod = "Data.Conduit.List"}
Reexport {as = "cb", mod = "Data.Conduit.Binary"}
```

### Parsing (megaparsec)

```haskell
Reexport {as = "parse", mod = "Text.Megaparsec"}
```

### compression

```haskell
Reexport {as = "tar", mod="Codec.Archive.Tar"}
Reexport {as = "tar", mod="Codec.Archive.Tar.Entry"}
Reexport {as = "tar", mod="Codec.Archive.Tar.Check"}
Reexport {as = "gzip", mod = "Codec.Compression.GZip"}
```

### Cmd-line tools: (ansi colors, optoparse-applicative)

  - Options.Applicative.Common:            Parser type and low-level parsing functionality.
  - Options.Applicative.Builder:           Utilities to build parsers out of basic primitives.
  - Options.Applicative.Builder.Completer: Common completion functions.
  - Options.Applicative.Extra:             Utilities to run parsers and display a help text.

```haskell
Reexport {as = "ansi", mod = "System.Console.ANSI"}
Reexport {as = "env", mod = "System.Environment"}
Reexport {as = "env", mod = "System.Exit"}
Reexport {as = "env", mod = "System.Process"}
Reexport {as = "opt", mod = "Options.Applicative.Common"}
Reexport {as = "opt", mod = "Options.Applicative.Builder"}
Reexport {as = "opt", mod = "Options.Applicative.Builder.Completer"}
Reexport {as = "opt", mod = "Options.Applicative.Extra"}
```

### Web: (wai, aeson, spock)

```haskell
Reexport {as = "wai", mod = "Network.Wai"}
Reexport {as = "wai", mod = "Network.Wai.Handler.Warp"}
Reexport {as = "wai", mod = "Network.HTTP.Types.Status"}
Reexport {as = "wai", mod = "Network.Wai.Middleware.RequestLogger"}
Reexport {as = "wai", mod = "Network.Wai.Middleware.Static"}

Reexport {as = "spock", mod = "Web.Spock.Digestive"}
Reexport {as = "spock", mod = "Web.Spock.Safe"}

Reexport {as = "ws", mod = "Network.WebSockets"}
Reexport {as = "ws", mod = "Network.WebSockets.Connection"}

Reexport {as = "http", mod = "Network.HTTP.Conduit"}

Reexport {as = "js", mod = "Data.Aeson"}
Reexport {as = "js", mod = "Data.Aeson.Types"}
Reexport {as = "js", mod = "Data.Aeson.Lens"}

Reexport {as = "uri", mod = "Network.URI"}
```
# js Data.Aeson.Types.Class not working because not exported, but I need the instances... !

### Lens (lens)

```haskell
Reexport {as = "lens", mod = "Lens.Micro.Platform"}
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
