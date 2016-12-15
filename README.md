## bindings-clp

This package provides a minimal low-level Haskell binding for the linear
programming solver [CLP](https://www.coin-or.org/Clp/).

More information can be found in the [source documentation](src/Clp.hs).

### Requirements 

This package depends on the `C/C++` interface of the [CLP
library](https://projects.coin-or.org/Clp). Furthermore, we assume the default
value `0` for `COIN_BIG_INDEX` when compiling Clp.

Tested successfully on a x86_64 GNU/Linux machine with `Stack lts-7.12`  and
`clp-1.15.10`.

