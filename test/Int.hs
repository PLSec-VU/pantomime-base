module Int (spec) where

import Common
import Pantomime.BuiltIn qualified as Pantomime

{-# ANN intAddComm (Theory axioms) #-}
intAddComm :: Int -> Int -> Pantomime.Bool
intAddComm (I# x) (I# y) = Pantomime.eqInt# (x +# y) (y +# x)

{-# ANN intAddIdent (Theory axioms) #-}
intAddIdent :: Int -> Pantomime.Bool
intAddIdent (I# x) = Pantomime.eqInt# (x +# 0#) x

{-# ANN intSubSelf (Theory axioms) #-}
intSubSelf :: Int -> Pantomime.Bool
intSubSelf (I# x) = Pantomime.eqInt# (x -# x) 0#

{-# ANN intMulComm (Theory axioms) #-}
intMulComm :: Int -> Int -> Pantomime.Bool
intMulComm (I# x) (I# y) = Pantomime.eqInt# (x *# y) (y *# x)

{-# ANN intInvalid (Theory axioms) #-}
intInvalid :: Int -> Pantomime.Bool
intInvalid (I# x) = Pantomime.eqInt# (x <# x) 1#

spec :: Spec
spec = describe "Int operations (via Int# axioms)" $ do
  it "addition is commutative" $
    $(pantomime 'intAddComm) `shouldBe` Nothing
  it "addition identity: x + 0 == x" $
    $(pantomime 'intAddIdent) `shouldBe` Nothing
  it "self-subtraction: x - x == 0" $
    $(pantomime 'intSubSelf) `shouldBe` Nothing
  it "multiplication is commutative" $
    $(pantomime 'intMulComm) `shouldBe` Nothing
  it "x < x is always false (invalid property)" $
    checkInvalid $(pantomime 'intInvalid)
