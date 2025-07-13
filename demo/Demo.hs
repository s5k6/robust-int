module Main ( main ) where

import Data.Text ( pack )
import Data.Word ( Word8, Word16 )

import qualified Data.Attoparsec.Text as A

import qualified Text.Parsec as P
import qualified Text.Parsec.String as P

import qualified Data.RobustInt.Attoparsec as RA
import qualified Data.RobustInt.Parsec as RP

import qualified Text.Parsec.Number as PN

{- This is used as an example in `README.md`. -}


parsecWord8 :: P.Parser Word8
parsecWord8 = read <$> P.many1 P.digit


main :: IO ()

main = do

  putStrLn "-- attoparsec --"

  print $ A.parseOnly (A.decimal :: A.Parser Word8) $ pack "298"
  print $ A.parseOnly (RA.bounded :: A.Parser Word8) $ pack "298"
  print $ A.parseOnly (RA.bounded :: A.Parser Word8) $ pack "198"
  print $ A.parseOnly (RA.bounded :: A.Parser Word16)  $ pack "298"


  putStrLn "-- parsec --"

  print $ P.runParser parsecWord8 () "" $  "298"
  print $ P.runParser (PN.decimal :: P.Parser Word8)  () "" "298"

  print $ P.runParser (RP.bounded :: P.Parser Word8)  () "" "298"
  print $ P.runParser (RP.bounded :: P.Parser Word8)  () "" "198"
  print $ P.runParser (RP.bounded :: P.Parser Word16) () "" "298"



  {- Do not use the following without protecting your system against
  memory exhaustion, e.g., use `ulimit -v 5000000` in bash. -}
  -- putStrLn "-- will exhaust memory --"
  -- print $ P.runParser parsecWord8 () "" $ repeat '1'
