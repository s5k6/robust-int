---
title: Correct parsers for bounded integral values
author: Stefan Klinger <https://www.stefan-klinger.de>
---


I've been bugged by the silent overflowing of integer parsers as
provided by `base`, `attoparsec`, and others.  I'd go so far as to
call it a bug when the user types `298` and the parser says `Right
42`.

Unfortunately, all parsing libraries I've looked at get this wrong.  A
[solution][4] is proposed below.

The following examples can be reproduced with

    $ git clone 'https://github.com/s5k6/robust-int.git'
    $ cd robust-int
    $ cabal repl robust-int:demo


Situation
---------

This is the current situation with [read][1] from base:

    > read "298" :: Word8
    42

And with [decimal][2] from attoparsec:

    > A.parseOnly (A.decimal :: A.Parser Word8) $ pack "298"
    Right 42

And the solution [usually suggested][5] for Parsec (which relies on
`read`):

    parsecWord8 :: P.Parser Word8
    parsecWord8 = read <$> P.many1 P.digit

    > P.runParser parsecWord8 () "" "298"
    Right 42

Even worse, the latter would rather exhaust memory than realise its
input is way out of bounds:

    > P.runParser parsecWord8 () "" $ repeat '1'
    ⊥

Also, some 3rd-party libraries get this wrong, e.g.,
[parsec3-numbers][6]:

    > P.runParser (PN.decimal :: P.Parser Word8)  () "" "298"
    Right 42

And [megaparsec][8], which is at least nice enough to warn about this
in its documentation:

    > M.parseMaybe (M.decimal :: M.Parsec () String Word8) "298"
    Just 42

I find this misses the point of a parser validating its input.


Solution
--------

It is [possible to implement][7] parsers for bounded integral types
which verify the bounds of the parsed value *while* parsing, and even
doing this without the use of a “bigger” type.

The idea is as follows:

As usual, we parse digits left to right, and collect the resulting
value in an accumulator `acc`, i.e., for each new digit `d`, the
accumulator is updated to

    base * acc + d

Nothing new up to here.  However, before we start parsing, calculate

    (lim, m) = upper_bound `divMod` base

and before updating the accumulator with another digit `d`, verify
that

    acc < lim || (acc == lim && d <= m)

which exactly guarantees that the accumulator will not overflow.  The
reason why this works is is easily seen by doing the example for
`Word16` in base 10:

    > (maxBound :: Word16) `divMod` 10
    (6553,5)
    > 10 * fst it + snd it
    65535

Complexity: This adds a modulo operation and two comparisons for every
literal being parsed, plus one comparison for every digit.  In order
to limit memory consumption, some comparison has to take place during
parsing, at least for limiting the number of digits consumed.  In
total, this does not look too expensive.

I have [implemented][4] this idea for `parsec` and `attoparsec` to
demonstrate the idea (only for decimal values).


What now?
---------

Obviously, this *should not be a another library*, trying to fix some
aspect of some other libraries.  My code is rather intended for
demonstration.  I'd prefer to help this idea migrate to the libraries
(`base`, `parsec`, `attoparsec`, …), where the correct parsers should
be.

Unfortunately, I got a bit lost when trying to track down the code of
`read` in the `base` package.  And I think I may have overengineered
my solution for attoparsec to accommodate different stream types.

Also, I get the impression that Haskell *library* code seems to be
written with a different mindset, a deeper understanding of GHC than
mine, i.e., more tailored to what the compiler will *actually do* when
using the code, trying not to spoil opportunities for optimisation.
And I'm not sure I'm up to that task.

So I'm asking for feedback on the proposed algorithm, my
implementation, and hints on where and how to get this into
established libraries.


Build instructions
==================

    $ cabal build
    $ cabal run demo

    $ cabal test
    $ cabal haddock


[1]: https://hackage.haskell.org/package/base-4.21.0.0/docs/Prelude.html#v:read
[2]: https://hackage.haskell.org/package/attoparsec-0.14.4/docs/Data-Attoparsec-ByteString-Char8.html#v:decimal
[3]: https://hackage.haskell.org/package/parsec-3.1.18.0/docs/Text-Parsec-Token.html#v:decimal
[4]: https://github.com/s5k6/robust-int
[5]: https://stackoverflow.com/questions/24171005/how-to-parse-an-integer-with-parsec
[6]: https://hackage.haskell.org/package/parsec3-numbers
[7]: https://github.com/s5k6/robust-int/blob/master/src/Data/RobustInt/Parsec.hs#L32-L52
[8]: https://hackage.haskell.org/package/megaparsec-9.7.0/docs/Text-Megaparsec-Char-Lexer.html#v:decimal
