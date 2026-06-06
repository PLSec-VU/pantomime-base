
module Int (spec) where

import Common
import Pantomime.BuiltIn qualified as Pantomime

-- {-# ANN intAddComm (Theory_disabled_disabled axioms) #-}
intAddComm :: Int -> Int -> Pantomime.Bool
intAddComm (I# x) (I# y) = Pantomime.eqInt# (x +# y) (y +# x)

-- {-# ANN intAddIdent (Theory_disabled_disabled axioms) #-}
intAddIdent :: Int -> Pantomime.Bool
intAddIdent (I# x) = Pantomime.eqInt# (x +# 0#) x

-- {-# ANN intSubSelf (Theory_disabled_disabled axioms) #-}
intSubSelf :: Int -> Pantomime.Bool
intSubSelf (I# x) = Pantomime.eqInt# (x -# x) 0#

-- {-# ANN intMulComm (Theory_disabled_disabled axioms) #-}
intMulComm :: Int -> Int -> Pantomime.Bool
intMulComm (I# x) (I# y) = Pantomime.eqInt# (x *# y) (y *# x)

-- {-# ANN intInvalid (Theory_disabled_disabled axioms) #-}
intInvalid :: Int -> Pantomime.Bool
intInvalid (I# x) = Pantomime.eqInt# (x <# x) 1#

spec :: Spec
spec = describe "Int operations (via Int# axioms)" $ do
  it "addition is commutative" $
    -- $(pantomime 'intAddComm) `shouldBe` Nothing
    todo
  it "addition identity: x + 0 == x" $
    -- $(pantomime 'intAddIdent) `shouldBe` Nothing
    todo
  it "self-subtraction: x - x == 0" $
    -- $(pantomime 'intSubSelf) `shouldBe` Nothing
    todo
  it "multiplication is commutative" $
    -- $(pantomime 'intMulComm) `shouldBe` Nothing
    todo
  it "x < x is always false (invalid property)" $
    -- checkInvalid $(pantomime 'intInvalid)
    todo
