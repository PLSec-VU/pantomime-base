module PtrTest (spec) where

import Common
import Data.ByteString.Internal (mallocByteString)
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import Foreign.Ptr (Ptr, castPtr, minusPtr, plusPtr)
import Pantomime.BuiltIn qualified as Pantomime
import System.IO.Unsafe (unsafePerformIO)
import Data.Word (Word8, Word16)

-- | plusPtr then minusPtr should round-trip: (p `plusPtr` n) `minusPtr` p == n.
{-# ANN ptrRoundTrip (Theory (axioms <> ioAxioms <> ptrAxioms)) #-}
ptrRoundTrip :: Ptr Word8 -> Int -> Pantomime.Bool
ptrRoundTrip p n = Pantomime.boolean (minusPtr (plusPtr p n) p == n)

-- | castPtr preserves the pointer offset: minusPtr (castPtr p) p == 0.
{-# ANN castPtrPreservesOffset (Theory (axioms <> ioAxioms <> ptrAxioms)) #-}
castPtrPreservesOffset :: Ptr Word8 -> Pantomime.Bool
castPtrPreservesOffset p = Pantomime.boolean $
  minusPtr (castPtr p :: Ptr Word16) p == 0

-- | plusPtr is additive: minusPtr (plusPtr (plusPtr p m) n) p == m + n.
{-# ANN plusPtrAdditive (Theory (axioms <> ioAxioms <> ptrAxioms)) #-}
plusPtrAdditive :: Ptr Word8 -> Int -> Int -> Pantomime.Bool
plusPtrAdditive p m n = Pantomime.boolean (minusPtr (plusPtr (plusPtr p m) n) p == m + n)

-- | mallocByteString then withForeignPtr: the materialized pointer has
-- offset 0 relative to itself.
{-# ANN mallocOffsetZero (Theory (axioms <> ioAxioms <> ptrAxioms)) #-}
mallocOffsetZero :: Pantomime.Bool
mallocOffsetZero = Pantomime.boolean $
  unsafePerformIO $ do
    fp <- mallocByteString 8 :: IO (ForeignPtr Word8)
    withForeignPtr fp $ \p -> return (minusPtr p p == 0)

-- | malloc + withForeignPtr + plusPtr: minusPtr (plusPtr p n) p == n
{-# ANN mallocPlusPtrInside (Theory (axioms <> ioAxioms <> ptrAxioms)) #-}
mallocPlusPtrInside :: Int -> Pantomime.Bool
mallocPlusPtrInside n = Pantomime.boolean $
  unsafePerformIO $ do
    fp <- mallocByteString 8 :: IO (ForeignPtr Word8)
    withForeignPtr fp $ \p -> return (minusPtr (plusPtr p n) p == n)


spec :: Spec
spec = describe "Pointer axioms" $ do
  it "plusPtr/minusPtr round-trip" $
    $(pantomime 'ptrRoundTrip) `shouldBe` Nothing
  it "castPtr preserves offset" $
    $(pantomime 'castPtrPreservesOffset) `shouldBe` Nothing
  it "plusPtr is additive" $
    $(pantomime 'plusPtrAdditive) `shouldBe` Nothing
  it "mallocByteString + withForeignPtr gives offset 0" $
    $(pantomime 'mallocOffsetZero) `shouldBe` Nothing
  it "plusPtr inside withForeignPtr round-trips" $
    $(pantomime 'mallocPlusPtrInside) `shouldBe` Nothing
