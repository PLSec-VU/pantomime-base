{-# OPTIONS_GHC -Wno-orphans #-}
module TestEncodeAnn (spec) where
import Common
import Data.ByteString (ByteString)
import Data.ByteString.Base64 qualified as B64
import Data.ByteString.Base64.Internal (withBS, mkBS)
import Data.Word (Word8)
import Foreign.ForeignPtr (ForeignPtr)
import Pantomime.BuiltIn qualified as Pantomime
import Pantomime.Base (axioms)
import Pantomime.IO (ioAxioms)
import Pantomime.Ptr (ptrAxioms, FakeForeignPtr (..))
import Pantomime.ByteString (byteStringAxioms)
import Unsafe.Coerce (unsafeCoerce)

-- | Verify that the actual B64.encode function produces output of length 4
-- for ALL single-byte inputs (all 256 Word8 values).
-- Constructs the input ByteString directly using FakeForeignPtr (the symbolic
-- representation), avoiding mallocByteString/mallocForeignPtrBytes which inline
-- to raw primops (newPinnedByteArray#, newMutVar#).
{-# ANN encodeLengthIs4 (Theory (axioms <> ioAxioms <> ptrAxioms <> byteStringAxioms)) #-}
encodeLengthIs4 :: Word8 -> Pantomime.Bool
encodeLengthIs4 b = Pantomime.boolean $
  withBS (B64.encode (mkInput b)) (\_ len -> return (len == 4))
  where
    mkInput :: Word8 -> ByteString
    mkInput _byte = mkBS mkFP 1

    mkFP :: ForeignPtr Word8
    mkFP = unsafeCoerce (FakeForeignPtr (Pantomime.fromInt# 3#) (Pantomime.fromInt# 1#))

spec :: Spec
spec = describe "real encode" $ do
  it "B64.encode (mkBS mkFP 1) has length 4 for all b" $
    $(pantomime 'encodeLengthIs4) `shouldBe` Nothing
