<h1 align="center">
    <a href="https://github.com/tonyday567/hrefactor">
        Haskell refactoring tools
    </a>
</h1>

<hr>

hrefactor is an experiment with haskell refactoring.

## Use & Develop

``` sh
$ git clone https://github.com/tonyday567/hrefactor.hs
$ cd hrefactor

$ cabal sandbox init
$ cabal configure --enable-tests
$ cabal install --dependencies-only --dry-run
$ cabal install --dependencies-only
$ cabal build
```

- put dist/build/hrefactor/hrefactor on the path
- put elisp/hrefactor.el where emacs can find it

try out `hrefactor-all`.

