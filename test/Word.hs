module Word (spec) where

import Common
import Pantomime.BuiltIn qualified as Pantomime

-- {-# ANN wordAddComm (Theory_disabled_disabled axioms) #-}
wordAddComm :: Word -> Word -> Pantomime.Bool
wordAddComm (W# x) (W# y) = Pantomime.eqWord# (x `plusWord#` y) (y `plusWord#` x)

-- {-# ANN wordAddIdent (Theory_disabled_disabled axioms) #-}
wordAddIdent :: Word -> Pantomime.Bool
wordAddIdent (W# x) = Pantomime.eqWord# (x `plusWord#` 0##) x

-- {-# ANN wordAndComm (Theory_disabled_disabled axioms) #-}
wordAndComm :: Word -> Word -> Pantomime.Bool
wordAndComm (W# x) (W# y) = Pantomime.eqWord# (x `and#` y) (y `and#` x)

-- {-# ANN wordInvalid (Theory_disabled_disabled axioms) #-}
wordInvalid :: Word -> Pantomime.Bool
wordInvalid (W# x) = Pantomime.eqInt# (x `ltWord#` x) 1#

spec :: Spec
spec = describe "Word operations (via Word# axioms)" $ do
  it "addition is commutative" $
    -- $(pantomime 'wordAddComm) `shouldBe` Nothing
    todo
  it "addition identity: x + 0 == x" $
    -- $(pantomime 'wordAddIdent) `shouldBe` Nothing
    todo
  it "AND is commutative" $
    -- $(pantomime 'wordAndComm) `shouldBe` Nothing
    todo
  it "x < x is always false (invalid property)" $
    -- checkInvalid $(pantomime 'wordInvalid)
    todo
