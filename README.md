some next steps
  - [ ] mention https://ghc.haskell.org/trac/ghc/wiki/ModuleReexports
  - [ ] mention backpack
  - [x] re-handle deprecated types
  - [x] show commented types above each definitions
  - [ ] export promoted kind aliases
  - [x] check how to deal with http://stackoverflow.com/questions/27895196/haskell-illegal-polymorphic-type makes it hard


# haskell-web-ide

All-in-one web-based ide (for stack projects)
```shell
$ cd myproject
$ haskell-web-ide
starting...
open http://localhost:3000/ to access the ide
```

# Features

#### Import manager :

##### cause I never want to manage my package list manually

  - Automatic syncing cabal file so all packages needed are imported
  - ctrl-i: textbox with autocompletion of all possible modules"

        on current packages:
            pkgname1 A.B.C f
            pkgname1 A.B.C.L f
        on other packages not listed yet on cabal:
            pkgname2 X.Y f
            pkgname2 X.Y f2
            pkgname7 F.G.H f3

  - button when unknown name error 

#### Automatic refactoring :

##### Cause I never want to refactor / lint code manually
  - applyrefact on save automatically
  - refactor imports
  future:
    - hindent
    - stylish

#### Module helper:
__cause I don't want to think about files anymore!__

  - renaming facilities
  - automatic sync-ing between cabal file and module hierarchy


# Open to contributions :)

```shell
stack install 
wai-devel -p src/Main.hs -f main
```

## Notes

- hoogle commands

```shell
git clone https://github.com/ndmitchell/hoogle /tmp/hoogle
cd /tmp/hoogle
stack init
stack install
stack exec -- hoogle generate --local --database=.stack-work/hoogle
stack exec -- hoogle $ARGS --database=.stack-work/hoogle
```

- hoogle should be installed from git (version 5)
- on snapshot change, hoogle db should be rebuilt. Until done, check here:https://github.com/commercialhaskell/stack/issues/55#issuecomment-155186311
- to install all tools, we should not rely on nightly but this repo should pick a nicely working snapshot
- if you want to have hyperlinked source, please, check https://github.com/commercialhaskell/stack/pull/1070

## FAQ

#### Why codemirror (and not ace or yyy)

codemirror have more keymaps (includig sublime text, and I like sublime text)
codemirror have is simple and have all required features:
  - https://codemirror.net/demo/lint.html
  - https://codemirror.net/demo/widget.html
  - Collaborative CodeMirror demo (CodeMirror + operational transforms) https://github.com/Operational-Transformation/ot.hs/tree/master/src/Control/OperationalTransformation


#### Why need to install 
https://github.com/commercialhaskell/stack/blob/6e3e8b1bbc27a744b0e486f93b94e2c9c5090a29/src/Stack/Build/Execute.hs#L1067
otherwise ("Warning: haddock not generating hyperlinked sources because 'HsColour' not found on PATH (use 'stack install hscolour' to install)."
