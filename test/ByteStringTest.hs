module ByteStringTest (spec) where

import Common
import Pantomime.BuiltIn qualified as Pantomime
import Data.ByteString qualified as BS

{-# ANN bsSingletonIndex (Theory axioms) #-}
bsSingletonIndex :: Word8 -> Pantomime.Bool
bsSingletonIndex w = Pantomime.boolean $ BS.index (BS.singleton w) 0 == w

{-# ANN bsNotNull (Theory axioms) #-}
bsNotNull :: BS.ByteString -> Pantomime.Bool
bsNotNull bs = Pantomime.boolean $ BS.index bs 0 == 0

spec :: Spec
spec = describe "ByteString operations" $ do
  it "index (singleton w) 0 == w" $ do
    $(pantomime 'bsSingletonIndex) `shouldBe` Nothing
  it "index isn't always 0 (counterexample)" $ do
    checkInvalid $(pantomime 'bsNotNull)
