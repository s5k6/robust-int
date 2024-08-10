module Main ( main ) where

import Data.RobustInt.Attoparsec

import Test.QuickCheck
import Data.Int
import Data.Word
import Data.String ( fromString )

import System.Environment ( getArgs )
import System.Exit ( exitFailure )
import System.Random ( Random )
import Data.Attoparsec.ByteString ( parseOnly, endOfInput )

import qualified Data.ByteString as B



-- Testing roundtrip for valid values

roundtrip_bounded :: (Show a, ParseBoundedInt a) => a -> Bool

roundtrip_bounded x = case parseOnly (bounded <* endOfInput) s of
  Right y -> x == y
  Left _ -> False
  where
    s :: B.ByteString
    s = fromString $ show x



prop_roundtrip_bounded_Int :: Int -> Bool
prop_roundtrip_bounded_Int = roundtrip_bounded

prop_roundtrip_bounded_Int8 :: Int8 -> Bool
prop_roundtrip_bounded_Int8 = roundtrip_bounded

prop_roundtrip_bounded_Int16 :: Int16 -> Bool
prop_roundtrip_bounded_Int16 = roundtrip_bounded

prop_roundtrip_bounded_Int32 :: Int32 -> Bool
prop_roundtrip_bounded_Int32 = roundtrip_bounded

prop_roundtrip_bounded_Int64 :: Int64 -> Bool
prop_roundtrip_bounded_Int64 = roundtrip_bounded

prop_roundtrip_bounded_Word :: Word -> Bool
prop_roundtrip_bounded_Word = roundtrip_bounded

prop_roundtrip_bounded_Word8 :: Word8 -> Bool
prop_roundtrip_bounded_Word8 = roundtrip_bounded

prop_roundtrip_bounded_Word16 :: Word16 -> Bool
prop_roundtrip_bounded_Word16 = roundtrip_bounded

prop_roundtrip_bounded_Word32 :: Word32 -> Bool
prop_roundtrip_bounded_Word32 = roundtrip_bounded

prop_roundtrip_bounded_Word64 :: Word64 -> Bool
prop_roundtrip_bounded_Word64 = roundtrip_bounded



-- Testing error detection for excessive values

newtype Excessive a = Excessive B.ByteString
  deriving Show


instance (Bounded a, Integral a) => Arbitrary (Excessive a) where
  arbitrary = do
    n :: Int <- arbitrary
    pure . Excessive . fromString . show
      $ if n >= 0
        then toInteger (maxBound :: a) + toInteger n + 1  -- Note 1
        else toInteger (minBound :: a) + toInteger n

{- Note 1: Adding 1 simply omits hitting 0 by shifting the positive
numbers one to the right.  Must be done in `Integer` domain to avoid
wrap around-}



exceed_bounded
  :: forall a. (Bounded a, ParseBoundedInt a)
  => Excessive a -> Bool

exceed_bounded (Excessive s) = case parseOnly (bounded <* endOfInput) s of
  Right (_ :: a) -> False  -- Note 2
  Left _ -> True

{- Note 2: ScopedTypeVariables are needed here to select the right
`bounded` function.  It is chosen by defining the value returned by
`parseOnly` to be `Either … a` on the left side of the first case
branch. -}



prop_exceed_bounded_Int :: Excessive Int -> Bool
prop_exceed_bounded_Int = exceed_bounded

prop_exceed_bounded_Int8 :: Excessive Int8 -> Bool
prop_exceed_bounded_Int8 = exceed_bounded

prop_exceed_bounded_Int16 :: Excessive Int16 -> Bool
prop_exceed_bounded_Int16 = exceed_bounded

prop_exceed_bounded_Int32 :: Excessive Int32 -> Bool
prop_exceed_bounded_Int32 = exceed_bounded

prop_exceed_bounded_Int64 :: Excessive Int64 -> Bool
prop_exceed_bounded_Int64 = exceed_bounded

prop_exceed_bounded_Word :: Excessive Word -> Bool
prop_exceed_bounded_Word = exceed_bounded

prop_exceed_bounded_Word8 :: Excessive Word8 -> Bool
prop_exceed_bounded_Word8 = exceed_bounded

prop_exceed_bounded_Word16 :: Excessive Word16 -> Bool
prop_exceed_bounded_Word16 = exceed_bounded

prop_exceed_bounded_Word32 :: Excessive Word32 -> Bool
prop_exceed_bounded_Word32 = exceed_bounded

prop_exceed_bounded_Word64 :: Excessive Word64 -> Bool
prop_exceed_bounded_Word64 = exceed_bounded



-- Testing error detection for invalid input values


{- This combines QuickCheck's `frequency` with `choose`: The element is
chosen from list of ranges, each range weighted by its size -}

foo :: [(Int, Int)] -> Gen Int

foo xs = frequency [ (u - l + 1, choose r) | r@(l,u) <- xs ]


data Invalid a = Invalid B.ByteString
  deriving Show



instance (Random a, Show a, Bounded a) => Arbitrary (Invalid a) where
  arbitrary = do
    x <- show <$> choose (minBound :: a, maxBound)
    (left, right) <- flip splitAt x <$> arbitrary
    c <- toEnum <$> if null left
                    then foo [(0,44),(46,47),(58,255)]
                    else foo [(0,47),(58,255)]

    pure . Invalid . fromString $ left ++ c : right



invalid_bounded
  :: forall a. (Bounded a, ParseBoundedInt a)
  => Invalid a -> Bool

invalid_bounded (Invalid s) = case parseOnly (bounded <* endOfInput) s of
  Right (_ :: a) -> False  -- Note 2
  Left _ -> True



prop_invalid_bounded_Int :: Invalid Int -> Bool
prop_invalid_bounded_Int = invalid_bounded

prop_invalid_bounded_Int8 :: Invalid Int8 -> Bool
prop_invalid_bounded_Int8 = invalid_bounded

prop_invalid_bounded_Int16 :: Invalid Int16 -> Bool
prop_invalid_bounded_Int16 = invalid_bounded

prop_invalid_bounded_Int32 :: Invalid Int32 -> Bool
prop_invalid_bounded_Int32 = invalid_bounded

prop_invalid_bounded_Int64 :: Invalid Int64 -> Bool
prop_invalid_bounded_Int64 = invalid_bounded

prop_invalid_bounded_Word :: Invalid Word -> Bool
prop_invalid_bounded_Word = invalid_bounded

prop_invalid_bounded_Word8 :: Invalid Word8 -> Bool
prop_invalid_bounded_Word8 = invalid_bounded

prop_invalid_bounded_Word16 :: Invalid Word16 -> Bool
prop_invalid_bounded_Word16 = invalid_bounded

prop_invalid_bounded_Word32 :: Invalid Word32 -> Bool
prop_invalid_bounded_Word32 = invalid_bounded

prop_invalid_bounded_Word64 :: Invalid Word64 -> Bool
prop_invalid_bounded_Word64 = invalid_bounded



return [] -- this is somehow needed for template haskell



main :: IO ()

main = do
  putStrLn ""

  args <- getArgs >>= \case
    [] -> pure myStdArgs
    [a, b] -> pure myStdArgs{ maxSuccess = read a, maxSize = read b }
    _ -> error "Specify none or both as arguments: maxSuccess maxSize"

  $(forAllProperties) (quickCheckWithResult args {-. verbose-} ) >>= \case
    True -> pure ()
    False -> exitFailure

  where
    myStdArgs = stdArgs{ maxSuccess = 1000 }
