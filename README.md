= Trivial example of GF for internationalization =

Usage:

    $ cabal-dev install
    ...
    $ ./cabal-dev/bin/foods-i8n
    FoodsEng
    FoodsIta
    foods-i8n: user error (Specify one of the above languages.)
    $ ./cabal-dev/bin/foods-i8n FoodsEng
    those pizzas are very fresh
    $ ./cabal-dev/bin/foods-i8n FoodsIta
    quelle pizze sono molto fresche



