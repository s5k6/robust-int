{-| Parse bounded integers.

The distinguishing feature of this library is, that the parser reports
an error if the limits of a bounded type are exceeded, rather than
wrapping around silently.

This module is for use with 'Text.Parsec'. -}

{-# LANGUAGE FlexibleContexts #-}

module Data.RobustInt.Parsec
  ( unsigned, signed, negative
  , ParseBoundedInt, bounded
  , nDigitInt
  ) where

import Data.RobustInt.Internal
import Data.Word
import Data.Int

import Text.Parsec ( ParsecT, Stream )
import Text.Parsec ( char, digit, notFollowedBy, (<|>) )



{- Parses one decimal digit into its value. -}

decimalDigit :: (Stream s m Char, Num a) => ParsecT s u m a

decimalDigit = fromIntegral . subtract (fromEnum '0') . fromEnum <$> digit



{-| Parse an integer represented by exactly @n@ digits.

This parser __does wrap around__, should the type not be large enough.

This parser does not fail if unconsumed digits remain, which allows
follow-up parsers to consume follow-up digits. -}

nDigitInt :: (Integral a, Stream s m Char) => Int -> ParsecT s u m a

nDigitInt = go 0
  where
    go !acc 0 = return acc
    go !acc r = do
      d <- decimalDigit
      go (10 * acc + d) (r - 1)



{-| @unsigned hi@ parses an unsigned decimal integer in the inclusive
range 0–@hi@.  A leading zero is not permitted.

@hi@ must be positive, the lower bound is 0.  The type @a@ must be big
enough to contain that range. -}

unsigned :: (Integral a, Stream s m Char) => a -> ParsecT s u m a

unsigned hi = zero <|> (decimalDigit >>= go)
  where
    zero = char '0' >> notFollowedBy digit >> pure 0

    go !acc = more <|> pure acc
      where
        more = do
          d <- decimalDigit
          acc < lim || (acc == lim && d <= m) ? go (10 * acc + d)
            $ fail "out of bounds"

    (lim, m) = hi `divMod` 10



{-| This is normally not used alone, unless you have a particular need
to parse strictly negative integers.

@negative lo@ parses a negative decimal integer in the inclusive range
@lo@–-1.  A zero after the @-@ is not permitted.

@lo@ must be negative, the upper bound is -1.  The type must be big
enough to contain that range.

Note, that it is __not__ straight forward to express @negative@ by
means of 'unsigned'.  Aside from the special case of catching @-0@,
the approach to use @char \'-\' >> negate \<$\> unsigned (negate
minBound)@ fails, because the negation of the lower bound may not be
(read: is usually not) within the upper bound, and thus wraps around,
e.g., incorrectly @negate (minBound :: Int8)@ → @-128@ due to the
upper bound of @127@. -}

negative :: (Integral a, Stream s m Char) => a -> ParsecT s u m a

negative lo = char '-' >> notFollowedBy (char '0') >> ndd >>= go
  where
    go !acc = more <|> pure acc
      where
        more = do
          d <- ndd
          lim < acc || (acc == lim && m <= d) ? go (10 * acc + d)
            $ fail "out of bounds"

    (lim, m) = lo `quotRem` 10

    ndd = negate <$> decimalDigit



{-| @signed lo hi@ parses a signed decimal integer in the inclusive
range @lo@–@hi@.  A leading zero (or one following the optional
leading @-@) is not allowed.

@lo@ must be negative, and @hi@ must be positive.  The type must be
big enough to contain that range. -}

signed :: (Integral a, Stream s m Char) => a -> a -> ParsecT s u m a

signed lo hi = unsigned hi <|> negative lo



{-| The expectation for instances of 'ParseBoundedInt' is, that
'bounded' parses a 'Bounded' decimal integer using the bounds of the
underlying type. -}

class Integral a => ParseBoundedInt a where

  {-| Parses a bounded integer, using the bounds set at instantiation. -}

  bounded :: Stream s m Char => ParsecT s u m a



instance ParseBoundedInt Word where bounded = unsigned maxBound
instance ParseBoundedInt Word8 where bounded = unsigned maxBound
instance ParseBoundedInt Word16 where bounded = unsigned maxBound
instance ParseBoundedInt Word32 where bounded = unsigned maxBound
instance ParseBoundedInt Word64 where bounded = unsigned maxBound

instance ParseBoundedInt Int where bounded = signed minBound maxBound
instance ParseBoundedInt Int8 where bounded = signed minBound maxBound
instance ParseBoundedInt Int16 where bounded = signed minBound maxBound
instance ParseBoundedInt Int32 where bounded = signed minBound maxBound
instance ParseBoundedInt Int64 where bounded = signed minBound maxBound
