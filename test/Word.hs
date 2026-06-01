module Word (spec) where

import Common
import Pantomime.BuiltIn qualified as Pantomime

{-# ANN wordAddComm (Theory axioms) #-}
wordAddComm :: Word -> Word -> Pantomime.Bool
wordAddComm (W# x) (W# y) = Pantomime.eqWord# (x `plusWord#` y) (y `plusWord#` x)

{-# ANN wordAddIdent (Theory axioms) #-}
wordAddIdent :: Word -> Pantomime.Bool
wordAddIdent (W# x) = Pantomime.eqWord# (x `plusWord#` 0##) x

{-# ANN wordAndComm (Theory axioms) #-}
wordAndComm :: Word -> Word -> Pantomime.Bool
wordAndComm (W# x) (W# y) = Pantomime.eqWord# (x `and#` y) (y `and#` x)

{-# ANN wordInvalid (Theory axioms) #-}
wordInvalid :: Word -> Pantomime.Bool
wordInvalid (W# x) = Pantomime.eqInt# (x `ltWord#` x) 1#

spec :: Spec
spec = describe "Word operations (via Word# axioms)" $ do
  it "addition is commutative" $ do
    $(pantomime 'wordAddComm) `shouldBe` Nothing
  it "addition identity: x + 0 == x" $ do
    $(pantomime 'wordAddIdent) `shouldBe` Nothing
  it "AND is commutative" $ do
    $(pantomime 'wordAndComm) `shouldBe` Nothing
  it "x < x is always false (invalid property)" $ do
    checkInvalid $(pantomime 'wordInvalid)
