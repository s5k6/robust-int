---
title: Correct parsers for bounded integral values
author: Stefan Klinger <https://www.stefan-klinger.de>
---

Correct parsers for bounded integral values
===========================================

Integer parsers for Parsec (on Text streams) and Attoparsec (on
ByteString and Text streams), that fail instead of wrapping around.

Assume a program needs to parse the decimal representation of a
bounded integral type.  You have chosen `Word8`, and use Attoparsec on
Text streams.  The following examples can be replicated by loading
GHCi with the appropriate set of modules.  Run `stack ghci
robust-int:demo`.


Current situation
-----------------

The `base` package provides `read`:

    > read "298" :: Word8
    42

Attoparsec provides `decimal`:

    > parseOnly (decimal :: Parser Word8) $ pack "298"
    Right 42

Both are wrong.  The user did not say "42".  The chosen type not big
enough for 298.


Improvement
-----------

This library gives you `bounded` (plus some more low-level parsers),
which correctly reports the mistake:

    > parseOnly (bounded :: Parser Word8) $ pack "298"
    Left "Failed reading: out of bounds"

This can be resolved by providing valid input:

    > parseOnly (bounded :: Parser Word8) $ pack "198"
    Right 198

Or by choosing an adequate type for the desired value:

    > parseOnly (bounded :: Parser Word16) $ pack "298"
    Right 298


Build instructions
==================

    $ stack build --test

    $ stack haddock
    $ firefox "$(stack path --local-doc-root)/index.html"
