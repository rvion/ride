## RIDE project : Ride Haskell like a Pro

:warning: This is WIP. 
Please, __don't post anything on reddit for now__.

Ride is a big project aiming to provide 

 1. A full feature web-based IDE, fast and as complete as possible.
      - [see planned features](/haskell-web-ide#haskell-web-ide)
 2. A tool `jetpack-gen` able to generate full-featured preludes with "prefixed" names
      - see an exemple input file: [imports.md](/imports.md)
      - see [the corresponding generated prelude](/jetpack/src)
        and [the full list of exported symbols](/jetpack/full-exported-symbol-list.txt)
 2. A full featured prelude exposing all necessary features for most project.


-------------

some next steps:
  - [ ] mention https://ghc.haskell.org/trac/ghc/wiki/ModuleReexports
  - [ ] mention backpack
  - [ ] cache module loading
  - [ ] export promoted kind aliases
  - [X] re-handle deprecated types
  - [x] show commented types above each definitions
  - [x] check how to deal with http://stackoverflow.com/questions/27895196/haskell-illegal-polymorphic-type makes it hard


