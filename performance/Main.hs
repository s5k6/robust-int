module Main where

import Criterion.Main
import Data.Word
import Data.Int

import qualified Text.Parsec as P
import qualified Data.RobustInt.Parsec as RP



{- the following functions use the builtin `read` function -}

readWord8 :: String -> Word8
readWord8 = read

readWord64 :: String -> Word64
readWord64 = read

readInt8 :: String -> Int8
readInt8 = read

readInt64 :: String -> Int64
readInt64 = read



{- the following functions use my more robust implementation -}

robustWord8 :: String -> Word8
robustWord8 s =
  case P.runParser RP.bounded () "" s of
    Right v -> v
    Left _ -> error "no parse"

robustWord64 :: String -> Word64
robustWord64 s =
  case P.runParser RP.bounded () "" s of
    Right v -> v
    Left _ -> error "no parse"

robustInt8 :: String -> Int8
robustInt8 s =
  case P.runParser RP.bounded () "" s of
    Right v -> v
    Left _ -> error "no parse"


robustInt64 :: String -> Int64
robustInt64 s =
  case P.runParser RP.bounded () "" s of
    Right v -> v
    Left _ -> error "no parse"



{- fixed, somewhat randomly chosen test values -}

bigNeg, bigPos, smallNeg, smallPos :: String

bigPos = "4611686018427386567"
bigNeg = '-' : bigPos

smallPos = "12"
smallNeg = '-' : smallPos


{- The benchmark applies only suitable input to the parsers, assuming
that most input a parser is about to read is correct.  I've chosen the
8bit and 64bit variants in order to cover the extreme ends of the
spectrum from short to long input. -}

main :: IO ()

main
  = defaultMain
    [ bgroup "unsigned"
      [ bench "readWord8"    $ nf readWord8    smallPos
      , bench "robustWord8"  $ nf robustWord8  smallPos
      , bench "readWord64"   $ nf readWord64   bigPos
      , bench "robustWord64" $ nf robustWord64 bigPos
      ]
    , bgroup "positive"
      [ bench "readInt8"    $ nf readInt8    smallPos
      , bench "robustInt8"  $ nf robustInt8  smallPos
      , bench "readInt64"   $ nf readInt64   bigPos
      , bench "robustInt64" $ nf robustInt64 bigPos
      ]
    , bgroup "negative"
      [ bench "readInt8"    $ nf readInt8    smallNeg
      , bench "robustInt8"  $ nf robustInt8  smallNeg
      , bench "readInt64"   $ nf readInt64   bigNeg
      , bench "robustInt64" $ nf robustInt64 bigNeg
      ]
    ]
