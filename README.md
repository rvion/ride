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

##### cause I don't want to think about files anymore !
  - renaming facilities
  - automatic sync-ing between cabal file and module hierarchy


# Open to contributions :)

```shell
stack install 
wai-devel -p src/Main.hs -f main
```

## FAQ
(and not ace or yyy)

codemirror have more keymaps (includig sublime text, and I like sublime text)
codemirror have is simple and have all required features:
  - https://codemirror.net/demo/lint.html
  - https://codemirror.net/demo/widget.html
  - etc.
