{-# LANGUAGE FlexibleContexts #-}

module ParseNumbers
  ( nDigitInt
  , limit
  , ParseBoundedInt, bounded
  ) where

import Common

import Text.Parsec ( ParsecT, Stream )
import Data.Text ( Text )
import Text.Parsec ( char, digit, notFollowedBy, (<?>), (<|>), try )
import Control.Monad ( when )
import Data.Word
import Data.Int


----------------------------------------------------------------------

{-
Test the parsed value.  Note: `check t p` uses `try p`, and succeeds
iff `t <$> p`.
-}

check :: Stream s m Char
      => (a -> Bool) -> ParsecT s u m a -> ParsecT s u m a

check t p = try $ do
  x <- p
  when (not $ t x) $ fail "invalid"
  return x


{-
Enforces limits on parsed value.  First argument is for error message.
-}

limit :: (Ord a, Show a, Stream s m Char)
      => String -> a -> a -> ParsecT s u m a -> ParsecT s u m a

limit what lo hi p =
  check ((>= lo) &&& (<= hi)) p
  <?>
  concat [what, " in range ", show lo, " to ", show hi]


{-
Parses one decimal digit into its value.
-}

decimalDigit :: (Stream s m Char, Num a) => ParsecT s u m a

decimalDigit = fromIntegral . subtract (fromEnum '0') . fromEnum <$> digit


{-
Parse an integer represented by exactly n digits.  This parser does
not fail if unconsumed digits remain, which allows followup parsers to
consume followup digits.
-}

nDigitInt :: (Integral a, Stream s m Char) => Int -> ParsecT s u m a

nDigitInt = go 0
  where
    go !acc 0 = return acc
    go !acc r = do
      d <- decimalDigit
      go (10 * acc + d) (r - 1)


----------------------------------------------------------------------
-- Bounded numbers

{-
The following parsers fail instead of wrapping around, when a numeral
exceeds the limits.
-}


{-
`unsigned hi` parses an unsigned decimal integer in the range 0
.. `hi`.  The type must be big enough to contain that range.  `hi`
must be positive, the lower bound is 0.  Leading zero is not
permitted.
-}

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


{-
This is normally not used alone, use `unsigned` or `signed` instead.

`negative lo` parses a negative decimal integer in the range `lo`
.. -1.  The type must be big enough to contain that range.  `lo` must
be negative, the upper bound is -1.  A zero after the `-` is not
permitted.

Note, that is not straight forward to express `negative` by means of
`unsigned`.  Aside from the special case of catching `-0`, the
approach to use `char '-' >> negate <$> unsigned (negate minBound)`
fails, because the negation of the lower bound may not be within the
upper bound, and thus wrap around, e.g., incorrectly `negate (minBound
:: Int8)` → `-128` due to the upper bound of `127`.
-}

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


{-
`signed lo hi` parses a signed decimal integer in the range `lo`
.. `hi`.  The type must be big enough to contain that range.  `lo`
must be negative, and `hi` must be positive.  A leading zero (or
following the optional leading `-`) is not allowed.
-}

signed :: (Integral a, Stream s m Char) => a -> a -> ParsecT s u m a

signed lo hi = unsigned hi <|> negative lo


{-
`bounded` parses a bounded decimal integer, and fails if the limits
are exceeded.
-}



class Integral a => ParseBoundedInt a where
  bounded :: Stream s m Char => ParsecT s u m a


instance ParseBoundedInt Word where
  bounded = unsigned (maxBound :: Word)

instance ParseBoundedInt Word8 where
  bounded = unsigned (maxBound :: Word8)

instance ParseBoundedInt Word16 where
  bounded = unsigned (maxBound :: Word16)

instance ParseBoundedInt Word32 where
  bounded = unsigned (maxBound :: Word32)

instance ParseBoundedInt Word64 where
  bounded = unsigned (maxBound :: Word64)


instance ParseBoundedInt Int where
  bounded = signed (minBound :: Int) (maxBound :: Int)

instance ParseBoundedInt Int8 where
  bounded = signed (minBound :: Int8) (maxBound :: Int8)

instance ParseBoundedInt Int16 where
  bounded = signed (minBound :: Int16) (maxBound :: Int16)

instance ParseBoundedInt Int32 where
  bounded = signed (minBound :: Int32) (maxBound :: Int32)

instance ParseBoundedInt Int64 where
  bounded = signed (minBound :: Int64) (maxBound :: Int64)
