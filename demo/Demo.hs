module Main ( main ) where

import Data.Text ( pack )
import Data.Attoparsec.Text ( Parser, parseOnly, decimal )
import Data.Word ( Word8, Word16 )
import Data.RobustInt.Attoparsec ( bounded )

{- This is used as an example in `README.md`. -}



main :: IO ()

main = do
  print $ parseOnly (decimal :: Parser Word8) $ pack "298"
  print $ parseOnly (bounded :: Parser Word8) $ pack "298"
  print $ parseOnly (bounded :: Parser Word8) $ pack "198"
  print $ parseOnly (bounded :: Parser Word16)  $ pack "298"
