# genart

This project is my take on generative art in Haskell.

The start is based on [Benjamin Kovach article](https://www.kovach.me/posts/2018-03-07-generating-art.html)


To build it I use `cabal`:

```shell
cabal sandbox init
cabal install alex
cabal install happy
cabal install --only-dependecies
cabal build
```



