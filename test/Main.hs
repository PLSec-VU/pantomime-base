module Main (main) where

import Test.Hspec

import qualified Int
import qualified Int8
import qualified Int16
import qualified Int32
import qualified Int64
import qualified Word
import qualified Word8
import qualified Word64
import qualified IntegerTest
import qualified BoolTest
import qualified ByteStringTest

import qualified IOExplain

main :: IO ()
main = hspec $ do
  IOExplain.spec
  {-
  Int.spec
...
  ByteStringTest.spec
  -}
