module ByteStringTest (spec) where

import Common
import Data.ByteString qualified as BS
import Pantomime.BuiltIn qualified as Pantomime

-- {-# ANN bsSingletonIndex (Theory_disabled_disabled axioms) #-}
bsSingletonIndex :: Word8 -> Pantomime.Bool
bsSingletonIndex w = Pantomime.boolean $ BS.index (BS.singleton w) 0 == w

-- {-# ANN bsNotNull (Theory_disabled_disabled axioms) #-}
bsNotNull :: BS.ByteString -> Pantomime.Bool
bsNotNull bs = Pantomime.boolean $ BS.index bs 0 == 0

spec :: Spec
spec = describe "ByteString operations" $ do
  it "index (singleton w) 0 == w" $
    -- $(pantomime 'bsSingletonIndex) `shouldBe` Nothing
    todo
  it "index isn't always 0 (counterexample)" $
    -- checkInvalid $(pantomime 'bsNotNull)
    todo
