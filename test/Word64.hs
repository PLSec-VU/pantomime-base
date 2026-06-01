module Word64 (spec) where

import Common
import Pantomime.BuiltIn qualified as Pantomime

{-# ANN word64AddComm (Theory axioms) #-}
word64AddComm :: Word64 -> Word64 -> Pantomime.Bool
word64AddComm (W64# x) (W64# y) = Pantomime.eqWord64# (x `plusWord64#` y) (y `plusWord64#` x)

{-# ANN word64Invalid (Theory axioms) #-}
word64Invalid :: Word64 -> Pantomime.Bool
word64Invalid (W64# x) = Pantomime.eqInt# (x `ltWord64#` x) 1#

spec :: Spec
spec = describe "Word64 operations" $ do
  it "addition is commutative" $ do
    $(pantomime 'word64AddComm) `shouldBe` Nothing
  it "x < x is always false (invalid property)" $ do
    checkInvalid $(pantomime 'word64Invalid)
