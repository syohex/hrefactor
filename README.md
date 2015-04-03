<h1 align="center">
    <a href="https://github.com/tonyday567/starter">
        Starter
    </a>
</h1>

<p align="center">
    s/Starter/ProjectName/
</p>

<hr>

Starter is a shell for a new haskell project.  It contains starter files for executables, library modules and hspec tests.

## Install

``` sh
$ cabal update
$ cabal install starter
```

## Use

``` sh
$ dist/build/starter/starter
```

## Develop

``` sh
$ git clone https://github.com/tonyday567/starter.hs
$ cd starter

$ cabal sandbox init
$ cabal configure --enable-tests
$ cabal install --dependencies-only --dry-run
$ cabal install --dependencies-only
$ cabal build
$ cabal test
```
