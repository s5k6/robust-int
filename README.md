
Run test:

    $ stack test

Check what's installed:

    $ find -L ~/{.ghc,.stack,.cabal}/ -name '*robust-int*' \
      -printf '%A+\t%p\n' | less -S

Install library:

    $ cabal install --lib

Verify:

    $ ghci
    > :m Data.RobustInt.Attoparsec
    > :t nDigitInt
    nDigitInt
      :: (Integral a, Data.RobustInt.Attoparsec.Generic i) =>
         Int -> attoparsec-0.14.4:Data.Attoparsec.Internal.Types.Parser i a
