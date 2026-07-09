module IOTest (spec) where

import Common
import Pantomime.BuiltIn qualified as Pantomime
import System.IO.Unsafe (unsafePerformIO)
import GHC.Base
import Data.IORef

ioRefPure :: a -> IO a
ioRefPure x = do
  ref <- newIORef undefined
  writeIORef ref x
  readIORef ref

{-# ANN testIO (Theory (axioms <> ioAxioms)) #-}
testIO :: Int -> Pantomime.Bool
testIO x = Pantomime.boolean (x == unsafePerformIO (ioRefPure x))

spec :: Spec
spec = describe "IO Explanation" $ do
  it "dumps the expression" $
    $(pantomime 'testIO) `shouldBe` Nothing
