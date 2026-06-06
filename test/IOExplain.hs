module IOExplain (spec) where

import Common
import Pantomime.BuiltIn qualified as Pantomime
import System.IO.Unsafe (unsafePerformIO)

{-# ANN testIO (Theory axioms) #-}
testIO :: Int -> Pantomime.Bool
testIO x = Pantomime.boolean (x == unsafePerformIO (return x))

spec :: Spec
spec = describe "IO Explanation" $ do
  it "dumps the expression" $
    $(pantomime 'testIO) `shouldBe` Nothing
