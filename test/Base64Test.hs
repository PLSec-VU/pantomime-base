{-# OPTIONS_GHC -Wno-orphans #-}

module Base64Test (spec) where

import Common
import Data.Bits ((.&.), (.|.), shiftL, shiftR)
import Data.ByteString qualified as BS
import Data.ByteString.Base64 qualified as B64
import Data.ByteString.Base64.Internal (peek8, poke8)
import Data.Word (Word8, Word32)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (plusPtr)
import GHC.ForeignPtr (mallocPlainForeignPtrBytes)
import Pantomime.BuiltIn qualified as Pantomime
import System.IO.Unsafe (unsafePerformIO)

-- | Verify the base64 'complete' branch arithmetic for a single byte.
-- For input byte 65 ('A'):
--   x = (65 .&. 0xfc) `shiftR` 2 = 16
--   y = (65 .&. 0x03) `shiftL` 4 = 16
-- These index into the alphabet: alphabet[16] = 'Q' (81)
{-# ANN completeArithmetic (Theory (axioms <> ioAxioms <> ptrAxioms <> byteStringAxioms)) #-}
completeArithmetic :: Pantomime.Bool
completeArithmetic = Pantomime.boolean $
  let aByte = 65 :: Word8
      a = fromIntegral aByte :: Word32
      x = (a .&. 0xfc) `shiftR` 2
      y = (a .&. 0x03) `shiftL` 4
  in x == 16 && y == 16

-- | Verify that peek8/poke8 from the actual base64-bytestring library
-- work correctly in the base64 alphabet access pattern:
--   poke8 (aptr + n) byte  then  peek8 (aptr + n) == byte
{-# ANN alphabetAccessPattern (Theory (axioms <> ioAxioms <> ptrAxioms <> byteStringAxioms)) #-}
alphabetAccessPattern :: Word8 -> Int -> Pantomime.Bool
alphabetAccessPattern val n = Pantomime.boolean $
  unsafePerformIO $ do
    fp <- mallocPlainForeignPtrBytes 64
    withForeignPtr fp $ \aptr -> do
      poke8 (aptr `plusPtr` n) val
      result <- peek8 (aptr `plusPtr` n)
      return (result == val)

-- | Verify the base64 complete branch pointer pattern for a single byte.
-- Pokes byte 65 into a source buffer, reads it via the actual library's
-- peek8, computes the base64 indices, writes alphabet characters and
-- padding into a dest buffer via poke8, then reads back and verifies
-- the output is "QQ==" (bytes [81, 81, 61, 61]).
--
-- This uses the actual Data.ByteString.Base64.Internal peek8/poke8
-- functions (axiomatized via term axioms in Pantomime.Ptr).
{-# ANN completeSingleByte (Theory (axioms <> ioAxioms <> ptrAxioms <> byteStringAxioms)) #-}
completeSingleByte :: Pantomime.Bool
completeSingleByte = Pantomime.boolean $
  unsafePerformIO $ do
    srcFp <- mallocPlainForeignPtrBytes 8
    dstFp <- mallocPlainForeignPtrBytes 8
    alphaFp <- mallocPlainForeignPtrBytes 64

    withForeignPtr srcFp $ \sptr -> do
      withForeignPtr dstFp $ \dptr -> do
        withForeignPtr alphaFp $ \aptr -> do
          poke8 sptr 65

          aByte <- peek8 sptr
          let aIdx = fromIntegral ((aByte .&. 0xfc) `shiftR` 2) :: Int
              bIdx = fromIntegral ((aByte .&. 0x03) `shiftL` 4) :: Int

          poke8 (aptr `plusPtr` aIdx) 81
          poke8 (aptr `plusPtr` bIdx) 81

          aChar <- peek8 (aptr `plusPtr` aIdx)
          bChar <- peek8 (aptr `plusPtr` bIdx)

          poke8 dptr aChar
          poke8 (dptr `plusPtr` 1) bChar
          poke8 (dptr `plusPtr` 2) 0x3d
          poke8 (dptr `plusPtr` 3) 0x3d

          r0 <- peek8 dptr
          r1 <- peek8 (dptr `plusPtr` 1)
          r2 <- peek8 (dptr `plusPtr` 2)
          r3 <- peek8 (dptr `plusPtr` 3)

          return (r0 == 81 && r1 == 81 && r2 == 0x3d && r3 == 0x3d)

spec :: Spec
spec = describe "base64-bytestring library verification" $ do
  it "complete branch arithmetic: (65 & 0xfc) >> 2 == 16, (65 & 0x03) << 4 == 16" $
    $(pantomime 'completeArithmetic) `shouldBe` Nothing
  it "alphabet access pattern: peek8/poke8 round-trip via plusPtr" $
    $(pantomime 'alphabetAccessPattern) `shouldBe` Nothing
  it "complete single byte: 65 → QQ==" $
    $(pantomime 'completeSingleByte) `shouldBe` Nothing
