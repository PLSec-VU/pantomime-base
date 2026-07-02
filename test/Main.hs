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

import qualified IOTest
import qualified Base64Test
import qualified PtrTest
import qualified TestEncodeAnn

main :: IO ()
main = hspec $ do
  IOTest.spec
  PtrTest.spec
  Base64Test.spec
  TestEncodeAnn.spec
  {-
  Int.spec
  ByteStringTest.spec
  -}
