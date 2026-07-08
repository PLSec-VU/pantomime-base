module PtrTest (spec) where

import Common
import Foreign.ForeignPtr (ForeignPtr)
import Pantomime.Ptr (mallocByteStringN, withForeignPtrN, plusPtrN)
import Foreign.Ptr (Ptr, castPtr, minusPtr)
import Pantomime.BuiltIn qualified as Pantomime
import Pantomime.Ptr (peekByte, pokeByte)
import System.IO.Unsafe (unsafePerformIO)
import Data.Word (Word8, Word16)

-- | plusPtr then minusPtr should round-trip: (p `plusPtr` n) `minusPtr` p == n.
{-# ANN ptrRoundTrip (Theory (axioms <> ioAxioms <> ptrAxioms)) #-}
ptrRoundTrip :: Ptr Word8 -> Int -> Pantomime.Bool
ptrRoundTrip p n = Pantomime.boolean (minusPtr (plusPtrN p n) p == n)

-- | castPtr preserves the pointer offset: minusPtr (castPtr p) p == 0.
{-# ANN castPtrPreservesOffset (Theory (axioms <> ioAxioms <> ptrAxioms)) #-}
castPtrPreservesOffset :: Ptr Word8 -> Pantomime.Bool
castPtrPreservesOffset p = Pantomime.boolean $
  minusPtr (castPtr p :: Ptr Word16) p == 0

-- | plusPtr is additive: minusPtr (plusPtr (plusPtr p m) n) p == m + n.
{-# ANN plusPtrAdditive (Theory (axioms <> ioAxioms <> ptrAxioms)) #-}
plusPtrAdditive :: Ptr Word8 -> Int -> Int -> Pantomime.Bool
plusPtrAdditive p m n = Pantomime.boolean (minusPtr (plusPtrN (plusPtrN p m) n) p == m + n)

-- | mallocPlainForeignPtrBytes then withForeignPtr: the materialized pointer has
-- offset 0 relative to itself.
{-# ANN mallocOffsetZero (Theory (axioms <> ioAxioms <> ptrAxioms)) #-}
mallocOffsetZero :: Pantomime.Bool
mallocOffsetZero = Pantomime.boolean $
  unsafePerformIO $ do
    fp <- mallocByteStringN 8 :: IO (ForeignPtr Word8)
    withForeignPtrN fp $ \p -> return (minusPtr p p == 0)

-- | malloc + withForeignPtr + plusPtr: minusPtr (plusPtr p n) p == n
{-# ANN mallocPlusPtrInside (Theory (axioms <> ioAxioms <> ptrAxioms)) #-}
mallocPlusPtrInside :: Int -> Pantomime.Bool
mallocPlusPtrInside n = Pantomime.boolean $
  unsafePerformIO $ do
    fp <- mallocByteStringN 8 :: IO (ForeignPtr Word8)
    withForeignPtrN fp $ \p -> return (minusPtr (plusPtrN p n) p == n)

-- | poke then peek at the same offset returns the written byte.
{-# ANN pokePeekRoundTrip (Theory (axioms <> ioAxioms <> ptrAxioms)) #-}
pokePeekRoundTrip :: Word8 -> Pantomime.Bool
pokePeekRoundTrip v = Pantomime.boolean $
  unsafePerformIO $ do
    fp <- mallocByteStringN 8 :: IO (ForeignPtr Word8)
    withForeignPtrN fp $ \p -> do
      pokeByte p v
      r <- peekByte p
      return (r == v)

-- | peek at a freshly malloc'd buffer returns 0 (zero-initialization).
{-# ANN mallocPeekZero (Theory (axioms <> ioAxioms <> ptrAxioms)) #-}
mallocPeekZero :: Pantomime.Bool
mallocPeekZero = Pantomime.boolean $
  unsafePerformIO $ do
    fp <- mallocByteStringN 8 :: IO (ForeignPtr Word8)
    withForeignPtrN fp $ \p -> do
      r <- peekByte p
      return (r == 0)

-- | poke at offset n, peek at the same offset: returns the written byte.
{-# ANN pokePeekAtOffset (Theory (axioms <> ioAxioms <> ptrAxioms)) #-}
pokePeekAtOffset :: Int -> Word8 -> Pantomime.Bool
pokePeekAtOffset n v = Pantomime.boolean $
  unsafePerformIO $ do
    fp <- mallocByteStringN 16 :: IO (ForeignPtr Word8)
    withForeignPtrN fp $ \p -> do
      pokeByte (plusPtrN p n) v
      r <- peekByte (plusPtrN p n)
      return (r == v)

-- | poke at offset 0, peek at offset 1: does NOT see the write (distinct cells).
{-# ANN pokePeekDistinctOffsets (Theory (axioms <> ioAxioms <> ptrAxioms)) #-}
pokePeekDistinctOffsets :: Word8 -> Pantomime.Bool
pokePeekDistinctOffsets v = Pantomime.boolean $
  unsafePerformIO $ do
    fp <- mallocByteStringN 16 :: IO (ForeignPtr Word8)
    withForeignPtrN fp $ \p -> do
      pokeByte p v
      r <- peekByte (plusPtrN p 1)
      return (r == 0)

-- | poke overwrites: poke v1, poke v2, peek returns v2.
{-# ANN pokeOverwrite (Theory (axioms <> ioAxioms <> ptrAxioms)) #-}
pokeOverwrite :: Word8 -> Word8 -> Pantomime.Bool
pokeOverwrite v1 v2 = Pantomime.boolean $
  unsafePerformIO $ do
    fp <- mallocByteStringN 8 :: IO (ForeignPtr Word8)
    withForeignPtrN fp $ \p -> do
      pokeByte p v1
      pokeByte p v2
      r <- peekByte p
      return (r == v2)

-- | poke at symbolic offset m, peek at symbolic offset n: if m /= n, the
-- write is not observed. Generalizes 'pokePeekDistinctOffsets' from a fixed
-- literal offset to arbitrary symbolic offsets.
{-# ANN pokePeekDistinctSymbolicOffsets (Theory (axioms <> ioAxioms <> ptrAxioms)) #-}
pokePeekDistinctSymbolicOffsets :: Int -> Int -> Word8 -> Pantomime.Bool
pokePeekDistinctSymbolicOffsets m n v = Pantomime.boolean $
  unsafePerformIO $ do
    fp <- mallocByteStringN 16 :: IO (ForeignPtr Word8)
    withForeignPtrN fp $ \p -> do
      pokeByte (plusPtrN p m) v
      r <- peekByte (plusPtrN p n)
      return ((m /= n) `implies` (r == 0))
  where
    implies False _ = True
    implies True  x = x

-- | poke at (p + m) + n, peek at p + (m + n): the same cell reached via two
-- different arithmetic paths is still the same cell.
{-# ANN pokePeekNestedArithmetic (Theory (axioms <> ioAxioms <> ptrAxioms)) #-}
pokePeekNestedArithmetic :: Int -> Int -> Word8 -> Pantomime.Bool
pokePeekNestedArithmetic m n v = Pantomime.boolean $
  unsafePerformIO $ do
    fp <- mallocByteStringN 16 :: IO (ForeignPtr Word8)
    withForeignPtrN fp $ \p -> do
      pokeByte (plusPtrN (plusPtrN p m) n) v
      r <- peekByte (plusPtrN p (m + n))
      return (r == v)

-- | poke through p, peek through castPtr (castPtr p): a round-trip cast
-- through another element type still observes the write, since castPtr
-- retypes the phantom without changing the underlying address.
{-# ANN pokePeekThroughCast (Theory (axioms <> ioAxioms <> ptrAxioms)) #-}
pokePeekThroughCast :: Word8 -> Pantomime.Bool
pokePeekThroughCast v = Pantomime.boolean $
  unsafePerformIO $ do
    fp <- mallocByteStringN 8 :: IO (ForeignPtr Word8)
    withForeignPtrN fp $ \p -> do
      pokeByte p v
      let p' = castPtr (castPtr p :: Ptr Word16) :: Ptr Word8
      r <- peekByte p'
      return (r == v)

-- | poke into buffer A at offset k, peek from a distinct buffer B at the
-- same offset k: does NOT see the write. Unlike 'pokePeekDistinctOffsets'
-- and 'pokePeekDistinctSymbolicOffsets', this separates cells by allocation
-- id rather than by offset within one allocation.
{-# ANN pokePeekDistinctAllocations (Theory (axioms <> ioAxioms <> ptrAxioms)) #-}
pokePeekDistinctAllocations :: Int -> Word8 -> Pantomime.Bool
pokePeekDistinctAllocations k v = Pantomime.boolean $
  unsafePerformIO $ do
    fpA <- mallocByteStringN 8 :: IO (ForeignPtr Word8)
    fpB <- mallocByteStringN 8 :: IO (ForeignPtr Word8)
    withForeignPtrN fpA $ \pa ->
      withForeignPtrN fpB $ \pb -> do
        pokeByte (plusPtrN pa k) v
        r <- peekByte (plusPtrN pb k)
        return (r == 0)

spec :: Spec
spec = describe "Pointer axioms" $ do
  it "plusPtr/minusPtr round-trip" $
    $(pantomime 'ptrRoundTrip) `shouldBe` Nothing
  it "castPtr preserves offset" $
    $(pantomime 'castPtrPreservesOffset) `shouldBe` Nothing
  it "plusPtr is additive" $
    $(pantomime 'plusPtrAdditive) `shouldBe` Nothing
  it "mallocPlainForeignPtrBytes + withForeignPtr gives offset 0" $
    $(pantomime 'mallocOffsetZero) `shouldBe` Nothing
  it "plusPtr inside withForeignPtr round-trips" $
    $(pantomime 'mallocPlusPtrInside) `shouldBe` Nothing
  it "poke then peek at same offset round-trips" $
    $(pantomime 'pokePeekRoundTrip) `shouldBe` Nothing
  it "peek at fresh malloc returns 0" $
    $(pantomime 'mallocPeekZero) `shouldBe` Nothing
  it "poke/peek at symbolic offset round-trips" $
    $(pantomime 'pokePeekAtOffset) `shouldBe` Nothing
  it "poke at 0 does not affect peek at 1" $
    $(pantomime 'pokePeekDistinctOffsets) `shouldBe` Nothing
  it "poke overwrites previous value" $
    $(pantomime 'pokeOverwrite) `shouldBe` Nothing
  it "poke/peek at distinct symbolic offsets does not alias" $
    $(pantomime 'pokePeekDistinctSymbolicOffsets) `shouldBe` Nothing
  it "poke/peek through nested arithmetic reaches the same cell" $
    $(pantomime 'pokePeekNestedArithmetic) `shouldBe` Nothing
  it "poke/peek through a round-trip castPtr sees the write" $
    $(pantomime 'pokePeekThroughCast) `shouldBe` Nothing
  it "poke/peek across distinct allocations does not alias" $
    $(pantomime 'pokePeekDistinctAllocations) `shouldBe` Nothing
