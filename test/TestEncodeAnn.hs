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

-- | Verify that B64.encode produces output of length 4 for ALL 1-byte inputs.
{-# ANN encodeLengthIs4 (Theory (axioms <> ioAxioms <> ptrAxioms <> byteStringAxioms)) #-}
encodeLengthIs4 :: Word8 -> Pantomime.Bool
encodeLengthIs4 b = Pantomime.boolean $
  withBS (B64.encode (mkBS mkFP 1)) (\_ len -> return (len == 4))
  where
    mkFP :: ForeignPtr Word8
    mkFP = unsafeCoerce (FakeForeignPtr (Pantomime.fromInt# 3#) (Pantomime.fromInt# 1#))

-- | Verify that B64.encode produces output of length 4 for ALL 2-byte inputs.
{-# ANN encodeLength2Is4 (Theory (axioms <> ioAxioms <> ptrAxioms <> byteStringAxioms)) #-}
encodeLength2Is4 :: Word8 -> Word8 -> Pantomime.Bool
encodeLength2Is4 a b = Pantomime.boolean $
  withBS (B64.encode (mkBS mkFP 2)) (\_ len -> return (len == 4))
  where
    mkFP :: ForeignPtr Word8
    mkFP = unsafeCoerce (FakeForeignPtr (Pantomime.fromInt# 3#) (Pantomime.fromInt# 2#))

-- | Verify that B64.encode produces output of length 4 for ALL 3-byte inputs.
{-# ANN encodeLength3Is4 (Theory (axioms <> ioAxioms <> ptrAxioms <> byteStringAxioms)) #-}
encodeLength3Is4 :: Word8 -> Word8 -> Word8 -> Pantomime.Bool
encodeLength3Is4 a b c = Pantomime.boolean $
  withBS (B64.encode (mkBS mkFP 3)) (\_ len -> return (len == 4))
  where
    mkFP :: ForeignPtr Word8
    mkFP = unsafeCoerce (FakeForeignPtr (Pantomime.fromInt# 3#) (Pantomime.fromInt# 3#))

spec :: Spec
spec = describe "real encode" $ do
  it "B64.encode (1 byte) has length 4 for all b" $
    $(pantomime 'encodeLengthIs4) `shouldBe` Nothing
  it "B64.encode (2 bytes) has length 4 for all a b" $
    $(pantomime 'encodeLength2Is4) `shouldBe` Nothing
  it "B64.encode (3 bytes) has length 4 for all a b c" $
    $(pantomime 'encodeLength3Is4) `shouldBe` Nothing
