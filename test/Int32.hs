
module Int32 (spec) where

import Common
import Pantomime.BuiltIn qualified as Pantomime

{-# ANN int32AddComm (Theory axioms) #-}
int32AddComm :: Int32 -> Int32 -> Pantomime.Bool
int32AddComm (I32# x) (I32# y) = Pantomime.eqInt32# (x `plusInt32#` y) (y `plusInt32#` x)

{-# ANN int32Invalid (Theory axioms) #-}
int32Invalid :: Int32 -> Pantomime.Bool
int32Invalid (I32# x) = Pantomime.eqInt# (x `ltInt32#` x) 1#

spec :: Spec
spec = describe "Int32 operations" $ do
  it "addition is commutative" $ do
    $(pantomime 'int32AddComm) `shouldBe` Nothing
  it "x < x is always false (invalid property)" $ do
    checkInvalid $(pantomime 'int32Invalid)
