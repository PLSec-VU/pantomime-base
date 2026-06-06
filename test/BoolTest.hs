module BoolTest (spec) where

import Common
import Pantomime.BuiltIn qualified as Pantomime

-- {-# ANN deMorganValid (Theory_disabled_disabled mempty) #-}
deMorganValid :: Bool -> Bool -> Pantomime.Bool
deMorganValid a b =
  let a' = Pantomime.boolean a
      b' = Pantomime.boolean b
  in Pantomime.iff
       (Pantomime.not (a' Pantomime.&& b'))
       (Pantomime.not a' Pantomime.|| Pantomime.not b')

-- {-# ANN fallacyInvalid (Theory_disabled_disabled mempty) #-}
fallacyInvalid :: Bool -> Bool -> Pantomime.Bool
fallacyInvalid a b =
  let a' = Pantomime.boolean a
      b' = Pantomime.boolean b
  in a' `Pantomime.implies` b'

spec :: Spec
spec = describe "Bool operations (no axioms)" $ do
  it "De Morgan's Law is valid" $
    -- $(pantomime 'deMorganValid) `shouldBe` Nothing
    todo
  it "implication is not a tautology" $
    -- checkInvalid $(pantomime 'fallacyInvalid)
    todo
