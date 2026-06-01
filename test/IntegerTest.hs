module IntegerTest (spec) where

import Common
import Pantomime.BuiltIn qualified as Pantomime

{-# ANN integerAddComm (Theory axioms) #-}
integerAddComm :: Pantomime.Integer -> Pantomime.Integer -> Pantomime.Bool
integerAddComm x y = Pantomime.ieq (Pantomime.iadd x y) (Pantomime.iadd y x)

{-# ANN integerSuccGt (Theory axioms) #-}
integerSuccGt :: Pantomime.Integer -> Pantomime.Bool
integerSuccGt x = Pantomime.ilt x (Pantomime.iadd x 1)

spec :: Spec
spec = describe "Integer operations" $ do
  it "addition is commutative" $ do
    $(pantomime 'integerAddComm) `shouldBe` Nothing
  it "x < x + 1 (no overflow for unbounded integers)" $ do
    $(pantomime 'integerSuccGt) `shouldBe` Nothing
