{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Pantomime.ByteString
  ( byteStringAxioms,
    ByteStringR,
  )
where

import Data.ByteString (ByteString)
import Data.ByteString.Base64 (alphabet)
import Data.Bits ((.&.), (.|.), shiftL, shiftR)
import Data.ByteString.Base64.Internal
  ( withBS,
    mkBS,
    mkEncodeTable,
    encodeWith,
    EncodeTable (ET),
    Padding (..),
    peek8,
    poke8,
  )
import Data.ByteString.Internal (mallocByteString)
import Data.Coerce (Coercible, coerce)
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import Foreign.Ptr (Ptr, plusPtr)
import GHC.Base (Int (..))
import GHC.Word (Word8 (..))
import GHC.Exts (IsList (..))
import Pantomime (PluginAxioms (..))
import Pantomime.BuiltIn qualified as Pantomime
import Pantomime.IO
  ( FakeHeap (..),
    FakeIO (..),
    FakeWorld (..),
    unsafePerformIOAxiom,
  )
import Pantomime.Ptr (FakeForeignPtr (..), FakePtr (..), mallocByteStringAxiom, plusPtrAxiom, withForeignPtrAxiom)
import System.IO.Unsafe (unsafePerformIO)
import Unsafe.Coerce (unsafeCoerce)

-- | Symbolic representation of a strict 'ByteString': a pair of a fake
-- foreign pointer (for heap access) and a length.
-- This mirrors the 'BS' constructor of 'ByteString' so that 'pushCoDataCon'
-- can push the 'BS' constructor through the 'ByteString ~ ByteStringR'
-- coercion.
data ByteStringR = BS_R !(FakeForeignPtr Word8) !Int

byteStringAxioms :: PluginAxioms
byteStringAxioms =
  PluginAxioms
    { typeAxioms =
        fromList
          [ (''ByteString, ''ByteStringR)
          ],
      termAxioms =
        [ ('withBS, 'withBSAxiom),
          ('mkBS, 'mkBSAxiom),
          ('alphabet, 'alphabetAxiom),
          ('mallocByteStringN, 'mallocByteStringAxiom),
          ('runIO, 'unsafePerformIOAxiom),
          ('plusPtrN, 'plusPtrAxiom),
          ('withForeignPtrN, 'withForeignPtrAxiom),
          ('mkEncodeTable, 'mkEncodeTableAxiom),
          ('encodeWith, 'encodeWithAxiom)
        ]
    }

-- | withBS :: ByteString -> (Ptr Word8 -> Int -> IO a) -> a
withBSAxiom
  :: forall a io
   . Coercible FakeIO io
  => ByteString
  -> (Ptr Word8 -> Int -> io a)
  -> a
withBSAxiom bs f =
  let BS_R fp slen = unsafeCoerce bs :: ByteStringR
      g :: FakeWorld -> (# FakeWorld, a #)
      g s =
        let fakePtr = FakePtr
              { ptrId = fptrId fp
              , ptrLen = fptrLen fp
              , ptrOff = 0
              } :: FakePtr Word8
            realPtr = unsafeCoerce fakePtr :: Ptr Word8
            FakeIO h = coerce (f realPtr slen) :: FakeIO a
        in h s
  in case g newWorld of (# _, a #) -> a
  where
    zeroByte = 0 :: Pantomime.BitVec 8
    zeroByteArray = Pantomime.aconst @Pantomime.Integer @(Pantomime.BitVec 8) zeroByte
    zeroHeapArray =
      Pantomime.aconst
        @Pantomime.Integer
        @(Pantomime.Array Pantomime.Integer (Pantomime.BitVec 8))
        zeroByteArray
    newWorld = FakeWorld
      { time = 0
      , refs = []
      , heap = FakeHeap {heapNext = 0, heapMem = zeroHeapArray}
      }

-- | mkBS :: ForeignPtr Word8 -> Int -> ByteString
mkBSAxiom :: ForeignPtr Word8 -> Int -> ByteString
mkBSAxiom fp n =
  unsafeCoerce (BS_R (unsafeCoerce fp :: FakeForeignPtr Word8) n) :: ByteString

-- | The 'alphabet' constant: the standard base64 alphabet as a ByteString.
-- Pointer id 0 is reserved for the alphabet buffer.
alphabetAxiom :: ByteString
alphabetAxiom =
  unsafeCoerce
    ( BS_R
        ( FakeForeignPtr
            { fptrId = 0
            , fptrLen = 64
            } :: FakeForeignPtr Word8
        )
        (64 :: Int)
    ) :: ByteString

-- | mkEncodeTable :: ByteString -> EncodeTable
-- The actual implementation builds a 4096-entry Word16 lookup table via
-- a loop. We axiomatize it to produce an EncodeTable with the alphabet
-- ForeignPtr (id=0) and a fresh ForeignPtr (id=1) for the encode table.
-- The 'complete' branch of encode only uses the alphabet pointer (via
-- 'aidx'), not the encode table.
{-# NOINLINE mallocByteStringN #-}
mallocByteStringN :: Int -> IO (ForeignPtr a)
mallocByteStringN = mallocByteString

mkEncodeTableAxiom :: ByteString -> EncodeTable
mkEncodeTableAxiom _bs =
  ET
    (runIO (mallocByteStringN 64))
    (runIO (mallocByteStringN 8192))


-- | encodeWith :: Padding -> EncodeTable -> ByteString -> ByteString
-- Axiomatized to replicate the 'complete' branch of the actual encodeWith
-- implementation. Uses withBS, peek8, poke8, mkBS (all axiomatized via
-- term axioms). Handles single-byte and two-byte inputs (non-recursive branch).
{-# NOINLINE runIO #-}
runIO :: IO a -> a
runIO = unsafePerformIO

{-# NOINLINE plusPtrN #-}
plusPtrN :: Ptr a -> Int -> Ptr b
plusPtrN = plusPtr

{-# NOINLINE withForeignPtrN #-}
withForeignPtrN :: ForeignPtr a -> (Ptr a -> IO b) -> IO b
withForeignPtrN = withForeignPtr

encodeWithAxiom :: Padding -> EncodeTable -> ByteString -> ByteString
encodeWithAxiom padding (ET alfaFP _encodeTableFP) bs =
  withBS bs $ \sptr slen -> do
    aptr <- withForeignPtrN alfaFP $ \p -> return (p :: Ptr Word8)
    let dfp = runIO (mallocByteStringN 4 :: IO (ForeignPtr Word8))
    withForeignPtrN dfp $ \dptr -> do
      let dlen = 4
          equals = 0x3d :: Word8
          doPad = padding == Padded
          aidxAlpha n = peek8 (aptr `plusPtrN` n)
      if slen > 0
        then do
          aByte <- peek8 sptr
          let aIdx = fromIntegral ((aByte .&. 0xfc) `shiftR` 2) :: Int
              bIdx = fromIntegral ((aByte .&. 0x03) `shiftL` 4) :: Int
          aChar <- aidxAlpha aIdx
          poke8 dptr aChar
          let twoMore = slen == 2
          if twoMore
            then do
              bByte <- peek8 (sptr `plusPtrN` 1)
              let b' = fromIntegral ((fromIntegral (bByte .&. 0xf0) `shiftR` 4 :: Int) .|. bIdx) :: Int
                  cIdx = fromIntegral ((bByte .&. 0x0f) `shiftL` 2) :: Int
              bChar <- aidxAlpha b'
              cChar <- aidxAlpha cIdx
              poke8 (dptr `plusPtrN` 1) bChar
              poke8 (dptr `plusPtrN` 2) cChar
              if doPad
                then do poke8 (dptr `plusPtrN` 3) equals; return (mkBS dfp dlen)
                else return (mkBS dfp (dlen - 1))
            else do
              bChar <- aidxAlpha bIdx
              poke8 (dptr `plusPtrN` 1) bChar
              if doPad
                then do
                  poke8 (dptr `plusPtrN` 2) equals
                  poke8 (dptr `plusPtrN` 3) equals
                  return (mkBS dfp dlen)
                else return (mkBS dfp (dlen - 2))
        else return (mkBS dfp 0)
