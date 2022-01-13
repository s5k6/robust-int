

module Robust.Attoparsec
  ( nDigitInt
  , limit
  , ParseBoundedInt, bounded, unsigned, negative, signed
  ) where

import Common

-- parsing UTF-8 encoded text
import Data.Attoparsec.Text ( Parser, char, string, digit, peekChar, (<?>) )

import Control.Monad ( when )
import Control.Applicative ( (<|>) )

import Data.Int ( Int8, Int16, Int32, Int64 )
import Data.Word ( Word8, Word16, Word32, Word64 )


----------------------------------------------------------------------
-- number parsing primitives

{-
Test the parsed value, succeeds iff `t <$> p`.
-}

check :: (a -> Bool) -> Parser a -> Parser a

check t p = do
  x <- p
  when (not $ t x) $ fail "check"
  return x



{-
Enforces limits on parsed value.
-}

limit :: Ord a => a -> a -> Parser a -> Parser a

limit lo hi = check $ (>= lo) &&& (<= hi)



{-
Parses one decimal digit into its value.
-}

decimalDigit :: Num a => Parser a

decimalDigit = fromIntegral . subtract (fromEnum '0') . fromEnum <$> digit



{-
Parse an integer represented by exactly n digits.  This parser does
not fail if unconsumed digits remain, which allows follow-up parsers
to consume follow-up digits.
-}

nDigitInt :: Integral a => Int -> Parser a

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

On out of bounds error, this parser does (intentionally) not backtrack
to just reading fewer digits.  This is achieved by hoisting the
decision of success in line A above the alternative in line B.
-}

unsigned :: Integral a => a -> Parser a

unsigned hi = decimalDigit >>= start
  where
    start 0 = pure 0 <* noMoreDigits
    start d | d <= hi = go d >>= maybe oob pure             -- A
            | otherwise = oob

    go !acc = (decimalDigit >>= cont) <|> pure (Just acc)   -- B
      where
        cont d | ok acc d = go (10 * acc + d)
               | otherwise = pure Nothing

    (lim, m) = hi `divMod` 10
    ok acc d = acc < lim || (acc == lim && d <= m)

    noMoreDigits = check (maybe True $ (< '0') ||| (> '9')) peekChar
      <?> "nondigit"

    oob = fail "out of bounds"


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

negative :: Integral a => a -> Parser a

negative lo = char '-' >> ndd >>= start
  where
    start 0 = fail "negative zero"
    start d | lo <= d = go d >>= maybe oob pure   -- A
            | otherwise = oob

    go !acc = (ndd >>= cont) <|> pure (Just acc)  -- B
      where
        cont d | ok acc d = go (10 * acc + d)
               | otherwise = pure Nothing

    (lim, m) = lo `quotRem` 10
    ok acc d = lim < acc || (acc == lim && m <= d)

    ndd = negate <$> decimalDigit

    oob = fail "out of bounds"



{-
`signed lo hi` parses a signed decimal integer in the range `lo`
.. `hi`.  The type must be big enough to contain that range.  `lo`
must be negative, and `hi` must be positive.  A leading zero (or
following the optional leading `-`) is not allowed.
-}

signed :: Integral a => a -> a -> Parser a

signed lo hi = unsigned hi <|> negative lo



{-
`bounded` parses a bounded decimal integer, and fails if the limits
are exceeded.  Note, that instantiation to `ParseBoundedInt` requires
`Integral` which is quite a lot.  For types with less context, maybe
rather use `signed` or `unsigned` instead.
-}

class Integral a => ParseBoundedInt a where
  bounded :: Parser a

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
