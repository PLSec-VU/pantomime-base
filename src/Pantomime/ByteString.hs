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
          ('unsafePerformIO, 'unsafePerformIOAxiom),
          ('runIO, 'unsafePerformIOAxiom),
          ('mkEncodeTable, 'mkEncodeTableAxiom)
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
{-# NOINLINE runIO #-}
runIO :: IO a -> a
runIO = unsafePerformIO

{-# NOINLINE mallocByteStringN #-}
mallocByteStringN :: Int -> IO (ForeignPtr a)
mallocByteStringN = mallocByteString

mkEncodeTableAxiom :: ByteString -> EncodeTable
mkEncodeTableAxiom _bs =
  ET
    (runIO (mallocByteStringN 64))
    (runIO (mallocByteStringN 8192))


