module Base64Test (spec) where

import Common
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString (ByteString)
import Data.ByteString.Internal (ByteString (..))
import Data.Word (Word8, Word32)
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import Foreign.Ptr (Ptr, plusPtr)
import GHC.ForeignPtr (mallocPlainForeignPtrBytes)
import Pantomime.BuiltIn qualified as Pantomime
import Pantomime.Ptr (peekByte, pokeByte)
import System.IO.Unsafe (unsafePerformIO)

-- =============================================================================
-- Replicated from Data.ByteString.Base64.Internal (not exposed by the library).
-- =============================================================================

peek8 :: Ptr Word8 -> IO Word8
peek8 = peekByte

poke8 :: Ptr Word8 -> Word8 -> IO ()
poke8 = pokeByte

peek8_32 :: Ptr Word8 -> IO Word32
peek8_32 = fmap fromIntegral . peek8

withBS :: ByteString -> (Ptr Word8 -> Int -> IO a) -> a
withBS (BS sfp slen) f = unsafePerformIO $
  withForeignPtr sfp $ \p -> f p slen

mkBS :: ForeignPtr Word8 -> Int -> ByteString
mkBS dfp n = BS dfp n

-- =============================================================================
-- Basic pointer-operation properties
-- =============================================================================

{-# ANN peek8_32RoundTrip (Theory (axioms <> ioAxioms <> ptrAxioms)) #-}
peek8_32RoundTrip :: Word8 -> Pantomime.Bool
peek8_32RoundTrip w = Pantomime.boolean $
  unsafePerformIO $ do
    fp <- mallocPlainForeignPtrBytes 8 :: IO (ForeignPtr Word8)
    withForeignPtr fp $ \p -> do
      poke8 p w
      r <- peek8_32 p
      return (r == fromIntegral w)

{-# ANN poke8Peek8RoundTrip (Theory (axioms <> ioAxioms <> ptrAxioms)) #-}
poke8Peek8RoundTrip :: Word8 -> Pantomime.Bool
poke8Peek8RoundTrip w = Pantomime.boolean $
  unsafePerformIO $ do
    fp <- mallocPlainForeignPtrBytes 8 :: IO (ForeignPtr Word8)
    withForeignPtr fp $ \p -> do
      poke8 p w
      r <- peek8 p
      return (r == w)

{-# ANN peek8_32FreshZero (Theory (axioms <> ioAxioms <> ptrAxioms)) #-}
peek8_32FreshZero :: Pantomime.Bool
peek8_32FreshZero = Pantomime.boolean $
  unsafePerformIO $ do
    fp <- mallocPlainForeignPtrBytes 8 :: IO (ForeignPtr Word8)
    withForeignPtr fp $ \p -> do
      r <- peek8_32 p
      return (r == 0)

{-# ANN poke8DistinctOffsets (Theory (axioms <> ioAxioms <> ptrAxioms)) #-}
poke8DistinctOffsets :: Word8 -> Pantomime.Bool
poke8DistinctOffsets w = Pantomime.boolean $
  unsafePerformIO $ do
    fp <- mallocPlainForeignPtrBytes 8 :: IO (ForeignPtr Word8)
    withForeignPtr fp $ \p -> do
      poke8 (plusPtr p 1) w
      r <- peek8 p
      return (r == 0)

{-# ANN encodeTripleCombine (Theory (axioms <> ioAxioms <> ptrAxioms)) #-}
encodeTripleCombine :: Word8 -> Word8 -> Word8 -> Pantomime.Bool
encodeTripleCombine i j k = Pantomime.boolean $
  unsafePerformIO $ do
    fp <- mallocPlainForeignPtrBytes 8 :: IO (ForeignPtr Word8)
    withForeignPtr fp $ \p -> do
      poke8 p i
      poke8 (plusPtr p 1) j
      poke8 (plusPtr p 2) k
      i' <- peek8_32 p
      j' <- peek8_32 (plusPtr p 1)
      k' <- peek8_32 (plusPtr p 2)
      let w = i' `shiftL` 16 .|. j' `shiftL` 8 .|. k'
      return (w == fromIntegral i `shiftL` 16 .|. fromIntegral j `shiftL` 8 .|. fromIntegral k)

-- =============================================================================
-- Full base64 encode: complete branch (1-byte tail, padded)
-- =============================================================================
--
-- Replicates 'complete' from Data.ByteString.Base64.Internal for the 1-byte
-- input case (the non-recursive tail of the encode loop). Exercises the full
-- encode path: peek from source, shift/bitwise ops, alphabet lookup via the
-- heap, poke to destination.
--
-- Source logic (complete, not twoMore, doPad=True):
--   a = (src .&. 0xfc) `shiftR` 2
--   b = (src .&. 0x03) `shiftL` 4
--   poke8 dp (aidx a)        -- alphabet[a]
--   poke8 (dp+1) (aidx b)    -- alphabet[b]
--   poke8 (dp+2) 0x3d        -- '='
--   poke8 (dp+3) 0x3d        -- '='

-- | Set up the base64 alphabet (A-Z a-z 0-9 + /) in a heap buffer.
setupAlphabet :: Ptr Word8 -> IO ()
setupAlphabet p = do
  poke8 p 65    -- A
  poke8 (plusPtr p 1) 66
  poke8 (plusPtr p 2) 67
  poke8 (plusPtr p 3) 68
  poke8 (plusPtr p 4) 69    -- E
  poke8 (plusPtr p 5) 70
  poke8 (plusPtr p 6) 71
  poke8 (plusPtr p 7) 72
  poke8 (plusPtr p 8) 73    -- I
  poke8 (plusPtr p 9) 74
  poke8 (plusPtr p 10) 75
  poke8 (plusPtr p 11) 76
  poke8 (plusPtr p 12) 77    -- M
  poke8 (plusPtr p 13) 78
  poke8 (plusPtr p 14) 79
  poke8 (plusPtr p 15) 80
  poke8 (plusPtr p 16) 81    -- Q
  poke8 (plusPtr p 17) 82
  poke8 (plusPtr p 18) 83
  poke8 (plusPtr p 19) 84
  poke8 (plusPtr p 20) 85    -- U
  poke8 (plusPtr p 21) 86
  poke8 (plusPtr p 22) 87
  poke8 (plusPtr p 23) 88
  poke8 (plusPtr p 24) 89    -- Y
  poke8 (plusPtr p 25) 90
  poke8 (plusPtr p 26) 97    -- a
  poke8 (plusPtr p 27) 98
  poke8 (plusPtr p 28) 99
  poke8 (plusPtr p 29) 100
  poke8 (plusPtr p 30) 101   -- e
  poke8 (plusPtr p 31) 102
  poke8 (plusPtr p 32) 103
  poke8 (plusPtr p 33) 104
  poke8 (plusPtr p 34) 105   -- i
  poke8 (plusPtr p 35) 106
  poke8 (plusPtr p 36) 107
  poke8 (plusPtr p 37) 108
  poke8 (plusPtr p 38) 109   -- m
  poke8 (plusPtr p 39) 110
  poke8 (plusPtr p 40) 111
  poke8 (plusPtr p 41) 112
  poke8 (plusPtr p 42) 113   -- q
  poke8 (plusPtr p 43) 114
  poke8 (plusPtr p 44) 115
  poke8 (plusPtr p 45) 116
  poke8 (plusPtr p 46) 117   -- u
  poke8 (plusPtr p 47) 118
  poke8 (plusPtr p 48) 119
  poke8 (plusPtr p 49) 120
  poke8 (plusPtr p 50) 121   -- y
  poke8 (plusPtr p 51) 122
  poke8 (plusPtr p 52) 48    -- 0
  poke8 (plusPtr p 53) 49
  poke8 (plusPtr p 54) 50
  poke8 (plusPtr p 55) 51
  poke8 (plusPtr p 56) 52    -- 4
  poke8 (plusPtr p 57) 53
  poke8 (plusPtr p 58) 54
  poke8 (plusPtr p 59) 55
  poke8 (plusPtr p 60) 56    -- 8
  poke8 (plusPtr p 61) 57
  poke8 (plusPtr p 62) 43    -- +
  poke8 (plusPtr p 63) 47    -- /

-- | The base64 'complete' branch for a 1-byte input, padded.
-- Replicates the logic from Data.ByteString.Base64.Internal.complete.
-- Writes 4 output bytes to the destination buffer.
encodeComplete1 :: Ptr Word8 -> Ptr Word8 -> Word8 -> IO ()
encodeComplete1 aptr dp src = do
  let aidx n = peek8 (aptr `plusPtr` fromIntegral n)
      a = (src .&. 0xfc) `shiftR` 2
      b = (src .&. 0x03) `shiftL` 4
  c0 <- aidx a
  c1 <- aidx b
  poke8 dp c0
  poke8 (plusPtr dp 1) c1
  poke8 (plusPtr dp 2) 0x3d
  poke8 (plusPtr dp 3) 0x3d

-- | Encoding 'A' (0x41 = 65) should produce "QQ==".
--   65 = 01000001
--   a = (65 .&. 0xfc) >> 2 = 64 >> 2 = 16  -> alphabet[16] = 'Q' (81)
--   b = (65 .&. 0x03) << 4 = 1 << 4 = 16   -> alphabet[16] = 'Q' (81)
--   padding: '=' (61), '=' (61)
{-# ANN encodeComplete1IsQQ (Theory (axioms <> ioAxioms <> ptrAxioms)) #-}
encodeComplete1IsQQ :: Pantomime.Bool
encodeComplete1IsQQ = Pantomime.boolean $
  unsafePerformIO $ do
    afp <- mallocPlainForeignPtrBytes 64 :: IO (ForeignPtr Word8)
    dfp <- mallocPlainForeignPtrBytes 4 :: IO (ForeignPtr Word8)
    withForeignPtr afp $ \aptr -> do
      setupAlphabet aptr
      withForeignPtr dfp $ \dp -> do
        encodeComplete1 aptr dp 65
        r0 <- peek8 dp
        r1 <- peek8 (plusPtr dp 1)
        r2 <- peek8 (plusPtr dp 2)
        r3 <- peek8 (plusPtr dp 3)
        return (r0 == 81 && r1 == 81 && r2 == 61 && r3 == 61)

-- | Encoding any byte: the first output byte equals
--   alphabet[(src .&. 0xfc) `shiftR` 2], read directly from the heap.
{-# ANN encodeComplete1FirstByte (Theory (axioms <> ioAxioms <> ptrAxioms)) #-}
encodeComplete1FirstByte :: Word8 -> Pantomime.Bool
encodeComplete1FirstByte src = Pantomime.boolean $
  unsafePerformIO $ do
    afp <- mallocPlainForeignPtrBytes 64 :: IO (ForeignPtr Word8)
    dfp <- mallocPlainForeignPtrBytes 4 :: IO (ForeignPtr Word8)
    withForeignPtr afp $ \aptr -> do
      setupAlphabet aptr
      withForeignPtr dfp $ \dp -> do
        encodeComplete1 aptr dp src
        r0 <- peek8 dp
        let a = (src .&. 0xfc) `shiftR` 2
        expected <- peek8 (aptr `plusPtr` fromIntegral a)
        return (r0 == expected)

-- | Encoding any byte: the second output byte equals
--   alphabet[(src .&. 0x03) `shiftL` 4], read directly from the heap.
{-# ANN encodeComplete1SecondByte (Theory (axioms <> ioAxioms <> ptrAxioms)) #-}
encodeComplete1SecondByte :: Word8 -> Pantomime.Bool
encodeComplete1SecondByte src = Pantomime.boolean $
  unsafePerformIO $ do
    afp <- mallocPlainForeignPtrBytes 64 :: IO (ForeignPtr Word8)
    dfp <- mallocPlainForeignPtrBytes 4 :: IO (ForeignPtr Word8)
    withForeignPtr afp $ \aptr -> do
      setupAlphabet aptr
      withForeignPtr dfp $ \dp -> do
        encodeComplete1 aptr dp src
        r1 <- peek8 (plusPtr dp 1)
        let b = (src .&. 0x03) `shiftL` 4
        expected <- peek8 (aptr `plusPtr` fromIntegral b)
        return (r1 == expected)

-- | Encoding any byte: bytes 3 and 4 are always '=' (0x3d) for padded mode.
{-# ANN encodeComplete1Padding (Theory (axioms <> ioAxioms <> ptrAxioms)) #-}
encodeComplete1Padding :: Word8 -> Pantomime.Bool
encodeComplete1Padding src = Pantomime.boolean $
  unsafePerformIO $ do
    afp <- mallocPlainForeignPtrBytes 64 :: IO (ForeignPtr Word8)
    dfp <- mallocPlainForeignPtrBytes 4 :: IO (ForeignPtr Word8)
    withForeignPtr afp $ \aptr -> do
      setupAlphabet aptr
      withForeignPtr dfp $ \dp -> do
        encodeComplete1 aptr dp src
        r2 <- peek8 (plusPtr dp 2)
        r3 <- peek8 (plusPtr dp 3)
        return (r2 == 0x3d && r3 == 0x3d)

spec :: Spec
spec = describe "base64-bytestring pointer operations" $ do
  it "peek8_32 (poke8 p w) == fromIntegral w" $
    $(pantomime 'peek8_32RoundTrip) `shouldBe` Nothing
  it "poke8/peek8 round-trips a single byte" $
    $(pantomime 'poke8Peek8RoundTrip) `shouldBe` Nothing
  it "peek8_32 reads zero from fresh buffer" $
    $(pantomime 'peek8_32FreshZero) `shouldBe` Nothing
  it "poke8 at offset 1 does not affect offset 0" $
    $(pantomime 'poke8DistinctOffsets) `shouldBe` Nothing
  it "encode triple combine: w = i<<16 | j<<8 | k" $
    $(pantomime 'encodeTripleCombine) `shouldBe` Nothing
  -- Full encode complete branch
  it "encode complete1 'A' produces QQ==" $
    $(pantomime 'encodeComplete1IsQQ) `shouldBe` Nothing
  it "encode complete1 first byte = alphabet[(src.&.0xfc)>>2]" $
    $(pantomime 'encodeComplete1FirstByte) `shouldBe` Nothing
  it "encode complete1 second byte = alphabet[(src.&.0x03)<<4]" $
    $(pantomime 'encodeComplete1SecondByte) `shouldBe` Nothing
  it "encode complete1 bytes 3,4 are '=' (padding)" $
    $(pantomime 'encodeComplete1Padding) `shouldBe` Nothing
