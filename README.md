<h1 align="center">
    <a href="https://github.com/tonyday567/hrefactor">
        Haskell refactoring tools
    </a>
</h1>

<hr>

hrefactor is an experiment with haskell refactoring.

The opinionated refactor amounts to:
- applying (some) warnings coming from haskell-flycheck - ghc & hlint on my config
- clean up imports
- clean up pragmas
- apply consistent separators
- apply a style from hindent

## Use & Develop

``` sh
    git clone https://github.com/tonyday567/hrefactor.hs
    cd hrefactor
    stack install && stack test
```

- put ~/.local/bin on the path
- put elisp/hrefactor.el where emacs can find it

try out `hrefactor-all`.

