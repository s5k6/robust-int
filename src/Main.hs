
module Main ( main ) where

import Data.Text ( pack )
import Text.Parsec.Text ( GenParser )
import Text.Parsec ( parse, eof )
import System.Environment ( getArgs )
import System.Exit ( exitFailure )
import Common
import ParseNumbers
import Data.Word
import Data.Int
import Control.Monad ( unless )
import Data.Proxy




----------------------------------------------------------------------
-- Tests


{-
Note: QuickCheck's `resize` limits the “size” of samples to `Int`,
just borderlien at what we want...
-}

test :: forall a . ParseBoundedInt a => Proxy a -> String -> Maybe a

test _ t = case parse (bounded <* eof) t (pack t) of
  Right v -> Just v
  Left _ -> Nothing


showTest :: forall a . (Show a, ParseBoundedInt a, Named a)
         => Proxy a -> String -> Maybe Integer -> IO ()

showTest ty text expected = do
  putStrLn $ unwords
    [ ok ? "ok" $ "FAILED", name ty, show text, maybe "none" show result
    ]
  unless ok $ fail "Failed"
  where
    result = test ty text
    ok = (fromIntegral <$> result) == expected



testUnsigned
  :: forall a. (ParseBoundedInt a, Show a, Bounded a, Named a)
  => Proxy a -> IO ()

testUnsigned ty = do

  mapM_ (\x -> showTest ty (show x) Nothing)
    [negate 5 :: Integer .. negate 1]

  mapM_ (\x -> showTest ty (show x) (Just x))
    $ [0 .. 10] ++ [hi - 5 .. hi]

  mapM_ (\x -> showTest ty (show x) Nothing)
    [fromIntegral hi + 1 :: Integer .. fromIntegral hi + 5]

  showTest ty "00" Nothing
  showTest ty "01" Nothing
  showTest ty "-" Nothing
  showTest ty "" Nothing

  where
    hi = fromIntegral (maxBound :: a)


testSigned
  :: forall a. (ParseBoundedInt a, Show a, Bounded a, Named a)
  => Proxy a -> IO ()

testSigned ty = do

  mapM_ (\x -> showTest ty (show x) Nothing)
    [fromIntegral lo - 5 :: Integer .. fromIntegral lo - 1]

  mapM_ (\x -> showTest ty (show x) (Just x))
    $ [lo .. lo + 5] ++ [-10 .. 10] ++ [hi - 5 .. hi]

  mapM_ (\x -> showTest ty (show x) Nothing)
    [fromIntegral hi + 1 :: Integer .. fromIntegral hi + 5]

  showTest ty "00" Nothing
  showTest ty "01" Nothing
  showTest ty "-0" Nothing
  showTest ty "-" Nothing
  showTest ty "" Nothing

  where
    lo = fromIntegral (minBound :: a)
    hi = fromIntegral (maxBound :: a)


class Named a where
  name :: Proxy a -> String

instance Named Word where name _ = "Word"
instance Named Word8 where name _ = "Word8"
instance Named Word16 where name _ = "Word16"
instance Named Word32 where name _ = "Word32"
instance Named Word64 where name _ = "Word64"

instance Named Int where name _ = "Int"
instance Named Int8 where name _ = "Int8"
instance Named Int16 where name _ = "Int16"
instance Named Int32 where name _ = "Int32"
instance Named Int64 where name _ = "Int64"


main :: IO ()
main = do
  testUnsigned (Proxy :: Proxy Word)
  testUnsigned (Proxy :: Proxy Word8)
  testUnsigned (Proxy :: Proxy Word16)
  testUnsigned (Proxy :: Proxy Word32)
  testUnsigned (Proxy :: Proxy Word64)
  testSigned (Proxy :: Proxy Int)
  testSigned (Proxy :: Proxy Int8)
  testSigned (Proxy :: Proxy Int16)
  testSigned (Proxy :: Proxy Int32)
  testSigned (Proxy :: Proxy Int64)
