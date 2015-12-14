## DGX


clone the repo with 

```shell
git clone https://github.com/rvion/dgx
cd dgx

```

Build the project with `stack`

[Click here to go to tjhe stack download page](https://www.stackage.org/stack)

After `stack` is in your path, run this command to get the compiler:

```
stack setup

```

```shell
stack build

```

auto-reload the project with `wai-devel`

```shell
wai-devel -p src/Main.hs -f main
stack build --ghc-options="-O0" --ghc-options="-dynamic -shared -fPIC" --skip-ghc-check --file-watch
```

You can install `wai-devel` with

```shell
stack install wai-devel
```