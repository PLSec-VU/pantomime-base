module Word8 (spec) where

import Common
import Pantomime.BuiltIn qualified as Pantomime

{-# ANN word8AddComm (Theory axioms) #-}
word8AddComm :: Word8 -> Word8 -> Pantomime.Bool
word8AddComm (W8# x) (W8# y) = Pantomime.eqWord8# (x `plusWord8#` y) (y `plusWord8#` x)

{-# ANN word8Invalid (Theory axioms) #-}
word8Invalid :: Word8 -> Pantomime.Bool
word8Invalid (W8# x) = Pantomime.eqInt# (x `ltWord8#` x) 1#

spec :: Spec
spec = describe "Word8 operations" $ do
  it "addition is commutative" $
    $(pantomime 'word8AddComm) `shouldBe` Nothing
  it "x < x is always false (invalid property)" $
    checkInvalid $(pantomime 'word8Invalid)
