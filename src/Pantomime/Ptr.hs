{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Pantomime.Ptr
  ( ptrAxioms,
    FakePtr (..),
    FakeForeignPtr (..),
    peekByte,
    pokeByte,
  )
where

import Data.ByteString.Internal (mallocByteString)
import Data.Coerce (Coercible, coerce)
import GHC.Word (Word8 (..))
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import Foreign.Ptr (Ptr, castPtr, minusPtr, plusPtr)
import GHC.Base (Int (I#))
import GHC.Exts (IsList (..))
import Pantomime (PluginAxioms (..))
import Pantomime.BuiltIn qualified as Pantomime
import Pantomime.IO
  ( FakeHeap (..),
    FakeIO (..),
    FakeWorld (..),
    nextWorld,
  )
import Unsafe.Coerce (unsafeCoerce)

-- | Word-sized bitvector, matching 'Int'/'Word' on the platform.
type PtrWord = Pantomime.BitVec Pantomime.PlatformWordSize

-- | A fake pointer: (id, length, offset). The phantom @a@ carries the element
-- type, matching 'Ptr's phantom role. Fields are word-sized bitvectors to
-- match 'Int' arithmetic and avoid cross-theory SMT conversions.
data FakePtr a = FakePtr
  { ptrId :: PtrWord
  , ptrLen :: PtrWord
  , ptrOff :: PtrWord
  }

-- | A fake foreign pointer: (id, length). No offset until 'withForeignPtr'
-- materializes a 'FakePtr'.
data FakeForeignPtr a = FakeForeignPtr
  { fptrId :: PtrWord
  , fptrLen :: PtrWord
  }

ptrAxioms :: PluginAxioms
ptrAxioms =
  PluginAxioms
    { typeAxioms =
        fromList
          [ (''Ptr, ''FakePtr),
            (''ForeignPtr, ''FakeForeignPtr)
          ],
      termAxioms =
        [ ('plusPtr, 'plusPtrAxiom),
          ('minusPtr, 'minusPtrAxiom),
          ('castPtr, 'castPtrAxiom),
          ('mallocByteString, 'mallocByteStringAxiom),
          ('withForeignPtr, 'withForeignPtrAxiom),
          ('peekByte, 'peekByteAxiom),
          ('pokeByte, 'pokeByteAxiom)
        ]
    }

-- | plusPtr :: Ptr a -> Int -> Ptr b
-- Bump the offset by n. Pure (no IO).
plusPtrAxiom
  :: forall a b ptr
   . Coercible FakePtr ptr
  => ptr a
  -> Int
  -> ptr b
plusPtrAxiom p n =
  let FakePtr {ptrId, ptrLen, ptrOff} = coerce p :: FakePtr a
      n' = Pantomime.fromInt# (case n of I# i# -> i#)
      result = FakePtr {ptrId, ptrLen, ptrOff = ptrOff + n'} :: FakePtr b
  in coerce result

-- | minusPtr :: Ptr a -> Ptr b -> Int
-- Offset difference. Pure.
minusPtrAxiom
  :: forall a b ptr
   . Coercible FakePtr ptr
  => ptr a
  -> ptr b
  -> Int
minusPtrAxiom p1 p2 =
  let FakePtr {ptrOff = o1} = coerce p1 :: FakePtr a
      FakePtr {ptrOff = o2} = coerce p2 :: FakePtr b
      diff = o1 - o2
  in I# (Pantomime.toInt# diff)

-- | castPtr :: Ptr a -> Ptr b
-- Retype the phantom; no runtime change.
castPtrAxiom
  :: forall a b ptr
   . Coercible FakePtr ptr
  => ptr a
  -> ptr b
castPtrAxiom p =
  let fake = coerce p :: FakePtr a
  in coerce (unsafeCoerce fake :: FakePtr b)

-- | mallocByteString :: Int -> IO (ForeignPtr a)
-- Allocate a fresh, zero-initialized byte array; return a fake foreign pointer.
mallocByteStringAxiom
  :: forall a io fptr
   . Coercible FakeIO io
  => Coercible FakeForeignPtr fptr
  => Int
  -> io (fptr a)
mallocByteStringAxiom n =
  let f :: FakeWorld -> (# FakeWorld, FakeForeignPtr a #)
      f s =
        let h = heap s
            newId = heapNext h
            zeroByte = 0 :: Pantomime.BitVec 8
            arr = Pantomime.aconst @Pantomime.Integer @(Pantomime.BitVec 8) zeroByte
            h' = h {heapNext = newId + 1, heapMem = Pantomime.astore @Pantomime.Integer @(Pantomime.Array Pantomime.Integer (Pantomime.BitVec 8)) (heapMem h) newId arr}
            s' = s {heap = h'}
            fptr = FakeForeignPtr
              { fptrId = Pantomime.i2bv @Pantomime.PlatformWordSize newId
              , fptrLen = Pantomime.fromInt# (case n of I# i# -> i#)
              }
        in (# nextWorld s', fptr #)
      m :: io (FakeForeignPtr a)
      m = coerce (FakeIO f)
  in coerce m

-- | withForeignPtr :: ForeignPtr a -> (Ptr a -> IO b) -> IO b
-- Materialize a fake pointer at offset 0 with the full length, run the
-- callback in the same FakeIO so heap effects thread through.
withForeignPtrAxiom
  :: forall a b io
   . Coercible FakeIO io
  => ForeignPtr a
  -> (Ptr a -> io b)
  -> io b
withForeignPtrAxiom fp k =
  let f :: FakeWorld -> (# FakeWorld, b #)
      f s =
        let FakeForeignPtr {fptrId, fptrLen} = unsafeCoerce fp :: FakeForeignPtr a
            fakePtr = FakePtr {ptrId = fptrId, ptrLen = fptrLen, ptrOff = 0} :: FakePtr a
            realPtr = unsafeCoerce fakePtr :: Ptr a
            FakeIO g = coerce (k realPtr) :: FakeIO b
        in g s
  in coerce (FakeIO f)

-- | Monomorphic Word8 peek wrapper, axiomatizable without a 'Storable'
-- constraint. Mirrors @peek8 = peek@ from base64-bytestring.
{-# NOINLINE peekByte #-}
peekByte :: Ptr Word8 -> IO Word8
peekByte = error "peekByte: axiom not resolved"

-- | Monomorphic Word8 poke wrapper, axiomatizable without a 'Storable'
-- constraint. Mirrors @poke8 = poke@ from base64-bytestring.
{-# NOINLINE pokeByte #-}
pokeByte :: Ptr Word8 -> Word8 -> IO ()
pokeByte = error "pokeByte: axiom not resolved"

-- | peekByte :: Ptr Word8 -> IO Word8
-- Read a single byte from the heap at the pointer's offset.
peekByteAxiom
  :: forall ptr io
   . Coercible FakePtr ptr
  => Coercible FakeIO io
  => ptr Word8
  -> io Word8
peekByteAxiom p =
  let f :: FakeWorld -> (# FakeWorld, Word8 #)
      f s =
        let FakePtr {ptrId, ptrOff} = coerce p :: FakePtr Word8
            arr = lookupHeap (heap s) (Pantomime.bvu2i ptrId)
            val = Pantomime.aselect @Pantomime.Integer @(Pantomime.BitVec 8) arr (Pantomime.bvu2i ptrOff)
        in (# nextWorld s, W8# (Pantomime.toWord8# val) #)
      m :: io Word8
      m = coerce (FakeIO f)
  in coerce m

-- | pokeByte :: Ptr Word8 -> Word8 -> IO ()
pokeByteAxiom
  :: forall ptr io
   . Coercible FakePtr ptr
  => Coercible FakeIO io
  => ptr Word8
  -> Word8
  -> io ()
pokeByteAxiom p (W8# w#) =
  let f :: FakeWorld -> (# FakeWorld, () #)
      f s =
        let FakePtr {ptrId, ptrOff} = coerce p :: FakePtr Word8
            h = heap s
            arr = lookupHeap h (Pantomime.bvu2i ptrId)
            arr' = Pantomime.astore @Pantomime.Integer @(Pantomime.BitVec 8) arr (Pantomime.bvu2i ptrOff) (Pantomime.fromWord8# w#)
            h' = updateHeap h (Pantomime.bvu2i ptrId) arr'
            s' = s {heap = h'}
        in (# nextWorld s', () #)
      m :: io ()
      m = coerce (FakeIO f)
  in coerce m

-- | Lookup the byte array for a given pointer id in the heap. Since the
-- heap is a symbolic array, this is a direct 'aselect'.
lookupHeap
  :: FakeHeap
  -> Pantomime.Integer
  -> Pantomime.Array Pantomime.Integer (Pantomime.BitVec 8)
lookupHeap h i = Pantomime.aselect @Pantomime.Integer @(Pantomime.Array Pantomime.Integer (Pantomime.BitVec 8)) (heapMem h) i

-- | Update the byte array for a given pointer id in the heap. Since the
-- heap is a symbolic array, this is a direct 'astore'.
updateHeap
  :: FakeHeap
  -> Pantomime.Integer
  -> Pantomime.Array Pantomime.Integer (Pantomime.BitVec 8)
  -> FakeHeap
updateHeap h i arr = h {heapMem = Pantomime.astore @Pantomime.Integer @(Pantomime.Array Pantomime.Integer (Pantomime.BitVec 8)) (heapMem h) i arr}
