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
import qualified IProuteTest
import qualified ContainersTest

main :: IO ()
main = hspec $ do
  IOTest.spec
  PtrTest.spec
  Base64Test.spec
  IProuteTest.spec
  ContainersTest.spec
  BoolTest.spec
  IntegerTest.spec
  Int.spec
  Int8.spec
  Int16.spec
  Int32.spec
  Int64.spec
  Word.spec
  Word8.spec
  Word64.spec
