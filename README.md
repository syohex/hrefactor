<h1 align="center">
    <a href="https://github.com/tonyday567/hrefactor">
        Haskell refactoring tools
    </a>
</h1>

<hr>

hrefactor is an experiment with haskell refactoring.

## Install

``` sh
$ cabal update
$ cabal install hrefactor
```

## Use

``` sh
$ dist/build/hrefactor/hrefactor
```

## Develop

``` sh
$ git clone https://github.com/tonyday567/hrefactor.hs
$ cd hrefactor

$ cabal sandbox init
$ cabal configure --enable-tests
$ cabal install --dependencies-only --dry-run
$ cabal install --dependencies-only
$ cabal build
$ cabal test
```

