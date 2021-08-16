
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


showTest :: forall a . (Show a, ParseBoundedInt a)
         => Proxy a -> String -> Maybe Integer -> IO ()

showTest ty text expected = do
  putStrLn $ unwords [ok ? "ok" $ "FAILED", text, show result]
  unless ok $ fail "Failed"
  where
    result = test ty text
    ok = (fromIntegral <$> result) == expected



testUnsigned
  :: forall a. (ParseBoundedInt a, Show a, Bounded a)
  => Proxy a -> IO ()

testUnsigned ty = do

  putStrLn "-- too low"
  mapM_ (\x -> showTest ty (show x) Nothing)
    [negate 5 :: Integer .. negate 1]

  putStrLn "-- inside limits"
  mapM_ (\x -> showTest ty (show x) (Just x))
    $ [0 .. 4] ++ [hi - 5 .. hi]

  putStrLn "-- too high"
  mapM_ (\x -> showTest ty (show x) Nothing)
    [fromIntegral hi + 1 :: Integer .. fromIntegral hi + 5]

  putStrLn "-- invalid"
  showTest ty "00" Nothing
  showTest ty "01" Nothing

  where
    hi = fromIntegral (maxBound :: a)


testSigned
  :: forall a. (ParseBoundedInt a, Show a, Bounded a)
  => Proxy a -> IO ()

testSigned ty = do

  putStrLn "-- too low"
  mapM_ (\x -> showTest ty (show x) Nothing)
    [fromIntegral lo - 5 :: Integer .. fromIntegral lo - 1]

  putStrLn "-- inside limits"
  mapM_ (\x -> showTest ty (show x) (Just x))
    $ [lo .. lo + 5] ++ [0 .. 5] ++ [hi - 5 .. hi]

  putStrLn "-- too high"
  mapM_ (\x -> showTest ty (show x) Nothing)
    [fromIntegral hi + 1 :: Integer .. fromIntegral hi + 5]

  putStrLn "-- invalid"
  showTest ty "00" Nothing
  showTest ty "01" Nothing
  showTest ty "-0" Nothing

  where
    lo = fromIntegral (minBound :: a)
    hi = fromIntegral (maxBound :: a)




main :: IO ()
main = do
  putStrLn "## Word"
  testUnsigned (Proxy :: Proxy Word)
  putStrLn "## Word8"
  testUnsigned (Proxy :: Proxy Word8)
  putStrLn "## Word16"
  testUnsigned (Proxy :: Proxy Word16)
  putStrLn "## Word32"
  testUnsigned (Proxy :: Proxy Word32)
  putStrLn "## Word64"
  testUnsigned (Proxy :: Proxy Word64)
  putStrLn "## Int"
  testSigned (Proxy :: Proxy Int)
  putStrLn "## Int8"
  testSigned (Proxy :: Proxy Int8)
  putStrLn "## Int16"
  testSigned (Proxy :: Proxy Int16)
  putStrLn "## Int32"
  testSigned (Proxy :: Proxy Int32)
  putStrLn "## Int64"
  testSigned (Proxy :: Proxy Int64)
