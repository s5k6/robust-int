module ParseNumbers
  ( nDigitInt
  , limit
  , ParseBoundedInt, bounded
  ) where

import Common

import Text.Parsec.Text ( GenParser )
import Text.Parsec ( char, digit, notFollowedBy, (<?>), (<|>), try )
import Control.Monad ( when )
import Data.Word
import Data.Int


----------------------------------------------------------------------

{-
Test the parsed value.  Note: `check t p` uses `try p`, and succeeds
iff `t <$> p`.
-}

check :: (a -> Bool) -> GenParser u a -> GenParser u a

check t p = try $ do
  x <- p
  when (not $ t x) $ fail "invalid"
  return x


{-
Enforces limits on parsed value.  First argument is for error message.
-}

limit :: (Ord a, Show a) => String -> a -> a -> GenParser u a -> GenParser u a

limit what lo hi p =
  check ((>= lo) &&& (<= hi)) p
  <?>
  concat [what, " in range ", show lo, " to ", show hi]


{-
Parses one decimal digit into its value.
-}

decimalDigit :: GenParser u Int

decimalDigit = subtract (fromEnum '0') . fromEnum <$> digit


{-
Parse an integer represented by exactly n digits.  This parser does
not fail if unconsumed digits remain, which allows followup parsers to
consume more digits..
-}

nDigitInt :: Integral a => Int -> GenParser u a

nDigitInt = go 0
  where
    go !acc 0 = return acc
    go !acc r = do
      d <- fromIntegral <$> decimalDigit
      go (10 * acc + d) (r - 1)



{- Note: `hi` must be positive, lower bound is 0. -}

unsigned :: Integral a => a -> GenParser u a

unsigned hi = zero <|> go 0
  where
    zero = char '0' >> notFollowedBy digit >> pure 0

    go !acc = more <|> pure acc
      where
        more = do
          d <- fromIntegral <$> decimalDigit
          acc < lim || (acc == lim && d <= m) ? go (10 * acc + d)
            $ fail "out of bounds"

    (lim, m) = hi `divMod` 10


{-
Note: `lo` must be negative, lower bound is 1.  Note, that is not
straight forward to express `negative` by means of `unsigned`.  The
approach of `char '-' >> negate <$> unsigned (negate minBound)` fails,
because the negation of the lower bound may not be within the upper
bound, and thus wrap around, e.g., `negate $ minBound :: Int8` →
`-128` which is incorrect.
-}

negative :: Integral a => a -> GenParser u a

negative lo = char '-' >> notFollowedBy (char '0') >> go 0
  where
    go !acc = more <|> pure acc
      where
        more = do
          d <- negate . fromIntegral <$> decimalDigit
          lim < acc || (acc == lim && m <= d) ? go (10 * acc + d)
            $ fail "out of bounds"

    (lim, m) = lo `quotRem` 10


{- Note: `lo` must be negative, `hi` must be positive. -}

signed :: Integral a => a -> a -> GenParser u a

signed lo hi = negative lo <|> unsigned hi




class Integral a => ParseBoundedInt a where
  bounded :: GenParser u a




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

