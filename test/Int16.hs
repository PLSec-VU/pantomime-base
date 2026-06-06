module Int16 (spec) where

import Common
import Pantomime.BuiltIn qualified as Pantomime

-- {-# ANN int16AddComm (Theory_disabled_disabled axioms) #-}
int16AddComm :: Int16 -> Int16 -> Pantomime.Bool
int16AddComm (I16# x) (I16# y) = Pantomime.eqInt16# (x `plusInt16#` y) (y `plusInt16#` x)

-- {-# ANN int16Invalid (Theory_disabled_disabled axioms) #-}
int16Invalid :: Int16 -> Pantomime.Bool
int16Invalid (I16# x) = Pantomime.eqInt# (x `ltInt16#` x) 1#

spec :: Spec
spec = describe "Int16 operations" $ do
  it "addition is commutative" $
    -- $(pantomime 'int16AddComm) `shouldBe` Nothing
    todo
  it "x < x is always false (invalid property)" $
    -- checkInvalid $(pantomime 'int16Invalid)
    todo
