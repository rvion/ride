# RIDE project : Ride Haskell like a Pro

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


### hack on this:

Those 2 commands should help
```shell
stack ghci repl
stack build --fast --file-watch
```

### packages: 

- '.' (ride) only usefull for playground. 
- 'haskell-web-ide'
- 'jetpack'
- 'jetpack-gen'
- 'reexport-demo'

