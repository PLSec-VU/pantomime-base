{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Main
  ( main
  ) where

import Test.Hspec
import Test.Hspec.Expectations (expectationFailure)

import Pantomime (Theory (..), pantomime)
import Pantomime.Base (axioms)
import Pantomime.BuiltIn qualified as Pantomime

import GHC.Exts
  ( Int#, Word#, Int8#, Int16#, Int32#, Int64#, Word8#, Word16#, Word32#, Word64#
  , (+#), (-#), (*#), (<#)
  , plusWord#, timesWord#, and#, ltWord#
  , plusInt8#, ltInt8#
  , plusInt16#, ltInt16#
  , plusInt32#, ltInt32#
  , plusInt64#, ltInt64#
  , plusWord8#, ltWord8#
  , plusWord64#, ltWord64#
  )
import GHC.Int (Int (I#), Int8 (I8#), Int16 (I16#), Int32 (I32#), Int64 (I64#))
import GHC.Word (Word (W#), Word8 (W8#), Word16 (W16#), Word32 (W32#), Word64 (W64#))

-- =============================================================================
-- Int Tests (via Int# axioms)
-- =============================================================================

{-# ANN intAddComm (Theory axioms) #-}
intAddComm :: Int -> Int -> Pantomime.Bool
intAddComm (I# x) (I# y) = Pantomime.eqInt# (x +# y) (y +# x)

{-# ANN intAddIdent (Theory axioms) #-}
intAddIdent :: Int -> Pantomime.Bool
intAddIdent (I# x) = Pantomime.eqInt# (x +# 0#) x

{-# ANN intSubSelf (Theory axioms) #-}
intSubSelf :: Int -> Pantomime.Bool
intSubSelf (I# x) = Pantomime.eqInt# (x -# x) 0#

{-# ANN intMulComm (Theory axioms) #-}
intMulComm :: Int -> Int -> Pantomime.Bool
intMulComm (I# x) (I# y) = Pantomime.eqInt# (x *# y) (y *# x)

{-# ANN intInvalid (Theory axioms) #-}
intInvalid :: Int -> Pantomime.Bool
intInvalid (I# x) = Pantomime.eqInt# (x <# x) 1#

-- =============================================================================
-- Word Tests (via Word# axioms)
-- =============================================================================

{-# ANN wordAddComm (Theory axioms) #-}
wordAddComm :: Word -> Word -> Pantomime.Bool
wordAddComm (W# x) (W# y) = Pantomime.eqWord# (x `plusWord#` y) (y `plusWord#` x)

{-# ANN wordAddIdent (Theory axioms) #-}
wordAddIdent :: Word -> Pantomime.Bool
wordAddIdent (W# x) = Pantomime.eqWord# (x `plusWord#` 0##) x

{-# ANN wordAndComm (Theory axioms) #-}
wordAndComm :: Word -> Word -> Pantomime.Bool
wordAndComm (W# x) (W# y) = Pantomime.eqWord# (x `and#` y) (y `and#` x)

{-# ANN wordInvalid (Theory axioms) #-}
wordInvalid :: Word -> Pantomime.Bool
wordInvalid (W# x) = Pantomime.eqInt# (x `ltWord#` x) 1#

-- =============================================================================
-- Int8 Tests
-- =============================================================================

{-# ANN int8AddComm (Theory axioms) #-}
int8AddComm :: Int8 -> Int8 -> Pantomime.Bool
int8AddComm (I8# x) (I8# y) = Pantomime.eqInt8# (x `plusInt8#` y) (y `plusInt8#` x)

{-# ANN int8Invalid (Theory axioms) #-}
int8Invalid :: Int8 -> Pantomime.Bool
int8Invalid (I8# x) = Pantomime.eqInt# (x `ltInt8#` x) 1#

-- =============================================================================
-- Int16 Tests
-- =============================================================================

{-# ANN int16AddComm (Theory axioms) #-}
int16AddComm :: Int16 -> Int16 -> Pantomime.Bool
int16AddComm (I16# x) (I16# y) = Pantomime.eqInt16# (x `plusInt16#` y) (y `plusInt16#` x)

{-# ANN int16Invalid (Theory axioms) #-}
int16Invalid :: Int16 -> Pantomime.Bool
int16Invalid (I16# x) = Pantomime.eqInt# (x `ltInt16#` x) 1#

-- =============================================================================
-- Int32 Tests
-- =============================================================================

{-# ANN int32AddComm (Theory axioms) #-}
int32AddComm :: Int32 -> Int32 -> Pantomime.Bool
int32AddComm (I32# x) (I32# y) = Pantomime.eqInt32# (x `plusInt32#` y) (y `plusInt32#` x)

{-# ANN int32Invalid (Theory axioms) #-}
int32Invalid :: Int32 -> Pantomime.Bool
int32Invalid (I32# x) = Pantomime.eqInt# (x `ltInt32#` x) 1#

-- =============================================================================
-- Int64 Tests
-- =============================================================================

{-# ANN int64AddComm (Theory axioms) #-}
int64AddComm :: Int64 -> Int64 -> Pantomime.Bool
int64AddComm (I64# x) (I64# y) = Pantomime.eqInt64# (x `plusInt64#` y) (y `plusInt64#` x)

{-# ANN int64Invalid (Theory axioms) #-}
int64Invalid :: Int64 -> Pantomime.Bool
int64Invalid (I64# x) = Pantomime.eqInt# (x `ltInt64#` x) 1#

-- =============================================================================
-- Word8 Tests
-- =============================================================================

{-# ANN word8AddComm (Theory axioms) #-}
word8AddComm :: Word8 -> Word8 -> Pantomime.Bool
word8AddComm (W8# x) (W8# y) = Pantomime.eqWord8# (x `plusWord8#` y) (y `plusWord8#` x)

{-# ANN word8Invalid (Theory axioms) #-}
word8Invalid :: Word8 -> Pantomime.Bool
word8Invalid (W8# x) = Pantomime.eqInt# (x `ltWord8#` x) 1#

-- =============================================================================
-- Word64 Tests
-- =============================================================================

{-# ANN word64AddComm (Theory axioms) #-}
word64AddComm :: Word64 -> Word64 -> Pantomime.Bool
word64AddComm (W64# x) (W64# y) = Pantomime.eqWord64# (x `plusWord64#` y) (y `plusWord64#` x)

{-# ANN word64Invalid (Theory axioms) #-}
word64Invalid :: Word64 -> Pantomime.Bool
word64Invalid (W64# x) = Pantomime.eqInt# (x `ltWord64#` x) 1#

-- =============================================================================
-- Integer Tests
-- =============================================================================

{-# ANN integerAddComm (Theory axioms) #-}
integerAddComm :: Pantomime.Integer -> Pantomime.Integer -> Pantomime.Bool
integerAddComm x y = Pantomime.ieq (Pantomime.iadd x y) (Pantomime.iadd y x)

{-# ANN integerSuccGt (Theory axioms) #-}
integerSuccGt :: Pantomime.Integer -> Pantomime.Bool
integerSuccGt x = Pantomime.ilt x (Pantomime.iadd x 1)

-- =============================================================================
-- Bool Tests (using empty axioms)
-- =============================================================================

{-# ANN deMorganValid (Theory mempty) #-}
deMorganValid :: Bool -> Bool -> Pantomime.Bool
deMorganValid a b =
  let a' = Pantomime.boolean a
      b' = Pantomime.boolean b
  in Pantomime.iff
       (Pantomime.not (a' Pantomime.&& b'))
       (Pantomime.not a' Pantomime.|| Pantomime.not b')

{-# ANN fallacyInvalid (Theory mempty) #-}
fallacyInvalid :: Bool -> Bool -> Pantomime.Bool
fallacyInvalid a b =
  let a' = Pantomime.boolean a
      b' = Pantomime.boolean b
  in a' `Pantomime.implies` b'

-- =============================================================================
-- Test Suite
-- =============================================================================

main :: IO ()
main = hspec $ do
  describe "Pantomime.Base axiom regression tests" $ do

    describe "Int operations (via Int# axioms)" $ do
      it "addition is commutative" $ do
        $(pantomime 'intAddComm) `shouldBe` Nothing
      it "addition identity: x + 0 == x" $ do
        $(pantomime 'intAddIdent) `shouldBe` Nothing
      it "self-subtraction: x - x == 0" $ do
        $(pantomime 'intSubSelf) `shouldBe` Nothing
      it "multiplication is commutative" $ do
        $(pantomime 'intMulComm) `shouldBe` Nothing
      it "x < x is always false (invalid property)" $ do
        checkInvalid $(pantomime 'intInvalid)

    describe "Word operations (via Word# axioms)" $ do
      it "addition is commutative" $ do
        $(pantomime 'wordAddComm) `shouldBe` Nothing
      it "addition identity: x + 0 == x" $ do
        $(pantomime 'wordAddIdent) `shouldBe` Nothing
      it "AND is commutative" $ do
        $(pantomime 'wordAndComm) `shouldBe` Nothing
      it "x < x is always false (invalid property)" $ do
        checkInvalid $(pantomime 'wordInvalid)

    describe "Int8 operations" $ do
      it "addition is commutative" $ do
        $(pantomime 'int8AddComm) `shouldBe` Nothing
      it "x < x is always false (invalid property)" $ do
        checkInvalid $(pantomime 'int8Invalid)

    describe "Int16 operations" $ do
      it "addition is commutative" $ do
        $(pantomime 'int16AddComm) `shouldBe` Nothing
      it "x < x is always false (invalid property)" $ do
        checkInvalid $(pantomime 'int16Invalid)

    describe "Int32 operations" $ do
      it "addition is commutative" $ do
        $(pantomime 'int32AddComm) `shouldBe` Nothing
      it "x < x is always false (invalid property)" $ do
        checkInvalid $(pantomime 'int32Invalid)

    describe "Int64 operations" $ do
      it "addition is commutative" $ do
        $(pantomime 'int64AddComm) `shouldBe` Nothing
      it "x < x is always false (invalid property)" $ do
        checkInvalid $(pantomime 'int64Invalid)

    describe "Word8 operations" $ do
      it "addition is commutative" $ do
        $(pantomime 'word8AddComm) `shouldBe` Nothing
      it "x < x is always false (invalid property)" $ do
        checkInvalid $(pantomime 'word8Invalid)

    describe "Word64 operations" $ do
      it "addition is commutative" $ do
        $(pantomime 'word64AddComm) `shouldBe` Nothing
      it "x < x is always false (invalid property)" $ do
        checkInvalid $(pantomime 'word64Invalid)

    describe "Integer operations" $ do
      it "addition is commutative" $ do
        $(pantomime 'integerAddComm) `shouldBe` Nothing
      it "x < x + 1 (no overflow for unbounded integers)" $ do
        $(pantomime 'integerSuccGt) `shouldBe` Nothing

    describe "Bool operations (no axioms)" $ do
      it "De Morgan's Law is valid" $ do
        $(pantomime 'deMorganValid) `shouldBe` Nothing
      it "implication is not a tautology" $ do
        checkInvalid $(pantomime 'fallacyInvalid)

-- | Assert that a counterexample was found and print it.
checkInvalid :: Maybe String -> Expectation
checkInvalid = \case
  Just ce -> do
    putStrLn ""
    putStrLn "Counterexample found:"
    putStrLn ce
    putStrLn ""
  Nothing -> expectationFailure "Expected a counterexample but assertion was valid"
