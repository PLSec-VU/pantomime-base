{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Pantomime.Ptr
  ( ptrAxioms,
    FakePtr (..),
    FakeForeignPtr (..),
  )
where

import GHC.Base (Int (I#))
import Data.ByteString.Internal (mallocByteString)
import Data.Coerce (Coercible, coerce)
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import Foreign.Ptr (Ptr, castPtr, minusPtr, plusPtr)
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
          ('withForeignPtr, 'withForeignPtrAxiom)
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
            h' = h {heapNext = newId + 1, heapMem = (newId, arr) : heapMem h}
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

-- | Lookup the byte array for a given pointer id. Falls back to a zero array
-- if not found (shouldn't happen with well-scoped allocations).
lookupHeap :: FakeHeap -> Pantomime.Integer -> Pantomime.Array Pantomime.Integer (Pantomime.BitVec 8)
lookupHeap h i = case lookup i (heapMem h) of
  Just arr -> arr
  Nothing -> Pantomime.aconst @Pantomime.Integer @(Pantomime.BitVec 8) (0 :: Pantomime.BitVec 8)

-- | Update (or insert) the array for a given pointer id in the heap memory list.
updateHeap
  :: [(Pantomime.Integer, Pantomime.Array Pantomime.Integer (Pantomime.BitVec 8))]
  -> Pantomime.Integer
  -> Pantomime.Array Pantomime.Integer (Pantomime.BitVec 8)
  -> [(Pantomime.Integer, Pantomime.Array Pantomime.Integer (Pantomime.BitVec 8))]
updateHeap [] i arr = [(i, arr)]
updateHeap ((j, a) : rest) i arr
  | j == i = (i, arr) : rest
  | otherwise = (j, a) : updateHeap rest i arr
