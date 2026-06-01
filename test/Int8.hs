
module Int8 (spec) where

import Common
import Pantomime.BuiltIn qualified as Pantomime

{-# ANN int8AddComm (Theory axioms) #-}
int8AddComm :: Int8 -> Int8 -> Pantomime.Bool
int8AddComm (I8# x) (I8# y) = Pantomime.eqInt8# (x `plusInt8#` y) (y `plusInt8#` x)

{-# ANN int8Invalid (Theory axioms) #-}
int8Invalid :: Int8 -> Pantomime.Bool
int8Invalid (I8# x) = Pantomime.eqInt# (x `ltInt8#` x) 1#

spec :: Spec
spec = describe "Int8 operations" $ do
  it "addition is commutative" $ do
    $(pantomime 'int8AddComm) `shouldBe` Nothing
  it "x < x is always false (invalid property)" $ do
    checkInvalid $(pantomime 'int8Invalid)
