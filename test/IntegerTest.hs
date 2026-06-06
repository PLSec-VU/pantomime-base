module IntegerTest (spec) where

import Common
import Pantomime.BuiltIn qualified as Pantomime

-- {-# ANN integerAddComm (Theory_disabled_disabled axioms) #-}
integerAddComm :: Pantomime.Integer -> Pantomime.Integer -> Pantomime.Bool
integerAddComm x y = Pantomime.ieq (Pantomime.iadd x y) (Pantomime.iadd y x)

-- {-# ANN integerSuccGt (Theory_disabled_disabled axioms) #-}
integerSuccGt :: Pantomime.Integer -> Pantomime.Bool
integerSuccGt x = Pantomime.ilt x (Pantomime.iadd x 1)

spec :: Spec
spec = describe "Integer operations" $ do
  it "addition is commutative" $
    -- $(pantomime 'integerAddComm) `shouldBe` Nothing
    todo
  it "x < x + 1 (no overflow for unbounded integers)" $
    -- $(pantomime 'integerSuccGt) `shouldBe` Nothing
    todo
