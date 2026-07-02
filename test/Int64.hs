module Int64 (spec) where

import Common
import Pantomime.BuiltIn qualified as Pantomime

{-# ANN int64AddComm (Theory axioms) #-}
int64AddComm :: Int64 -> Int64 -> Pantomime.Bool
int64AddComm (I64# x) (I64# y) = Pantomime.eqInt64# (x `plusInt64#` y) (y `plusInt64#` x)

{-# ANN int64Invalid (Theory axioms) #-}
int64Invalid :: Int64 -> Pantomime.Bool
int64Invalid (I64# x) = Pantomime.eqInt# (x `ltInt64#` x) 1#

spec :: Spec
spec = describe "Int64 operations" $ do
  it "addition is commutative" $
    $(pantomime 'int64AddComm) `shouldBe` Nothing
  it "x < x is always false (invalid property)" $
    checkInvalid $(pantomime 'int64Invalid)
