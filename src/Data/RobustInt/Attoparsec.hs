{-| Parse bounded integers.

The distinguishing feature of this library is, that the parser reports
an error if the limits of a bounded type are exceeded, rather than
wrapping around silently.

This module is for use with 'Data.Attoparsec', supporting streams of
type 'Data.Text' and 'Data.ByteString', both being instances of the
__hidden class__ @Generic@:

@
instance Generic 'T.Text'
instance Generic 'B.ByteString'
@
-}

module Data.RobustInt.Attoparsec
  ( unsigned, signed, negative
  , ParseBoundedInt, bounded
  , nDigitInt
  ) where

import Data.Attoparsec.Internal.Types ( Parser )
import Control.Monad ( when )
import Control.Applicative ( (<|>), liftA2 )
import Data.Int ( Int8, Int16, Int32, Int64 )
import Data.Word ( Word8, Word16, Word32, Word64 )

-- parsing UTF-8 encoded text
import qualified Data.Attoparsec.Text as T
import qualified Data.Text as T

-- parsing byte strings
import qualified Data.Attoparsec.ByteString as B
import qualified Data.ByteString as B



{- Lifted boolean operators. -}

infixr 3 <&&>

(<&&>) :: Applicative f => f Bool -> f Bool -> f Bool

(<&&>) = liftA2 (&&)



infixr 2 <||>

(<||>) :: Applicative f => f Bool -> f Bool -> f Bool

(<||>) = liftA2 (||)



{- Test the parsed value, succeeds iff @t <$> p@. -}

check :: (a -> Bool) -> Parser i a -> Parser i a

check t p = do
  x <- p
  when (not $ t x) $ fail "check"
  return x



{- Enforces limits on parsed value. -}

limit :: Ord a => a -> a -> Parser i a -> Parser i a

limit lo hi = check $ (>= lo) <&&> (<= hi)



{-| This class provides the minimal properties needed from the input
stream to be able to parse integes from it. -}

class Generic i where

  {-| Parse and convert one decimal digit. -}
  decimalDigit :: Num a => Parser i a

  {-| Assure no more digits follow. -}
  noMoreDigits :: Parser i ()

  {-| Parse a minus sign. -}
  minus :: Parser i ()


instance Generic T.Text where
  decimalDigit =
    fromIntegral . subtract (fromEnum '0') . fromEnum <$> T.digit

  noMoreDigits =
    check (maybe True $ (< '0') <||> (> '9')) T.peekChar >> pure ()

  minus =
    T.char '-' >> pure ()


instance Generic B.ByteString where
  decimalDigit =
    fromIntegral . subtract 48 <$> limit 48 57 B.anyWord8

  noMoreDigits =
    check (maybe True $ (< 48) <||> (> 57)) B.peekWord8 >> pure ()

  minus =
    B.word8 45 >> pure ()



{-| Parse an integer represented by exactly @n@ digits.

This parser __does wrap around__, should the type not be large enough.

This parser does not fail if unconsumed digits remain, which allows
follow-up parsers to consume follow-up digits. -}

nDigitInt :: (Integral a, Generic i) => Word -> Parser i a

nDigitInt = go 0
  where
    go !acc 0 = return acc
    go !acc r = do
      d <- decimalDigit
      go (10 * acc + d) (r - 1)



{-| @unsigned hi@ parses an unsigned decimal integer in the inclusive
range 0–@hi@.  A leading zero is not permitted.

@hi@ must be positive, the lower bound is 0.  The type @a@ must be big
enough to contain that range.

On out of bounds error, this parser does intentionally not backtrack
to just reading fewer digits. -}

unsigned :: (Integral a, Generic i) => a -> Parser i a

unsigned hi = decimalDigit >>= start
  where
    start 0 = pure 0 <* noMoreDigits
    start d | d <= hi = go d >>= maybe oob pure             -- A (Note 1)
            | otherwise = oob

    go !acc = (decimalDigit >>= cont) <|> pure (Just acc)   -- B (Note 1)
      where
        cont d | ok acc d = go (10 * acc + d)
               | otherwise = pure Nothing

    (lim, m) = hi `divMod` 10
    ok acc d = acc < lim || (acc == lim && d <= m)

    oob = fail "out of bounds"



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

negative :: (Generic i, Integral a) => a -> Parser i a

negative lo = minus >> ndd >>= start
  where
    start 0 = fail "negative zero"
    start d | lo <= d = go d >>= maybe oob pure   -- A (Note 1)
            | otherwise = oob

    go !acc = (ndd >>= cont) <|> pure (Just acc)  -- B (Note 1)
      where
        cont d | ok acc d = go (10 * acc + d)
               | otherwise = pure Nothing

    (lim, m) = lo `quotRem` 10
    ok acc d = lim < acc || (acc == lim && m <= d)

    ndd = negate <$> decimalDigit

    oob = fail "out of bounds"



{-| @signed lo hi@ parses a signed decimal integer in the inclusive
range @lo@–@hi@.  A leading zero (or one following the optional
leading @-@) is not allowed.

@lo@ must be negative, and @hi@ must be positive.  The type must be
big enough to contain that range. -}

signed :: (Generic i, Integral a) => a -> a -> Parser i a

signed lo hi = unsigned hi <|> negative lo



{-| The expectation for instances of 'ParseBoundedInt' is, that
'bounded' parses a 'Bounded' decimal integer using the bounds of the
underlying type. -}

class Integral a => ParseBoundedInt a where

  {-| Parses a bounded integer, using the bounds set at instantiation. -}

  bounded :: Generic i => Parser i a



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



{- Notes

Note 1: On out of bounds error, these parsers do not backtrack to just
reading fewer digits.  This is achieved by hoisting the decision of
success in line A above the alternative in line B.

-}
