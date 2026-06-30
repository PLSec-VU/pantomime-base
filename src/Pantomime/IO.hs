{-# LANGUAGE UnboxedTuples #-}

module Pantomime.IO
  ( ioAxioms,
    FakeWorld (..),
    FakeHeap (..),
    FakeIO (..),
    FakeIORef (..),
    nextWorld,
  )
where

import Data.Coerce (Coercible, coerce)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import GHC.Base (Any, RealWorld, bindIO, returnIO)
import GHC.Exts (IsList (..))
import GHC.Internal.Base qualified as GHC.Internal.Base
import GHC.Internal.IO qualified as GHC.Internal.IO
import GHC.Internal.IO.Unsafe qualified as GHC.Internal.IO.Unsafe
import GHC.IO qualified as GHC.IO
import GHC.IO.Unsafe qualified as GHC.IO.Unsafe
import Pantomime (PluginAxioms (..))
import Pantomime.BuiltIn qualified as Pantomime
import System.IO.Unsafe (unsafePerformIO)
import Unsafe.Coerce (unsafeCoerce)

ioAxioms :: PluginAxioms
ioAxioms =
  PluginAxioms
    { typeAxioms =
        fromList
          [ (''RealWorld, ''FakeWorld),
            (''IO, ''FakeIO),
            (''IORef, ''FakeIORef)
          ],
      termAxioms =
        [ ('unsafePerformIO, 'unsafePerformIOAxiom),
          ('GHC.IO.unsafePerformIO, 'unsafePerformIOAxiom),
          ('GHC.IO.Unsafe.unsafePerformIO, 'unsafePerformIOAxiom),
          ('GHC.Internal.IO.unsafePerformIO, 'unsafePerformIOAxiom),
          ('GHC.Internal.IO.Unsafe.unsafePerformIO, 'unsafePerformIOAxiom),
          ('returnIO, 'returnIOAxiom),
          ('GHC.Internal.Base.returnIO, 'returnIOAxiom),
          ('bindIO, 'bindIOAxiom),
          ('GHC.Internal.Base.bindIO, 'bindIOAxiom),
          ('newIORef, 'newIORefAxiom),
          ('readIORef, 'readIORefAxiom),
          ('writeIORef, 'writeIORefAxiom)
        ]
    }

data FakeIORef a = FakeIORef
  { refID :: Pantomime.Integer
  , value :: a
  }

-- | The symbolic heap. Maps pointer id -> byte array. Threaded through
-- 'FakeWorld' so pointer IO operations (peek/poke/malloc) can mutate it.
data FakeHeap = FakeHeap
  { heapNext :: Pantomime.Integer
  , heapMem :: [(Pantomime.Integer, Pantomime.Array Pantomime.Integer (Pantomime.BitVec 8))]
  }
data FakeWorld = FakeWorld
  { time :: Pantomime.Integer
  , refs :: [Any]
  , heap :: FakeHeap
  }

newtype FakeIO a = FakeIO (FakeWorld -> (# FakeWorld, a #))

nextWorld :: FakeWorld -> FakeWorld
nextWorld wrld@(FakeWorld {..}) = wrld {time = time + 1}

unsafePerformIOAxiom
  :: forall io a
   . Coercible FakeIO io
  => io a
  -> a
unsafePerformIOAxiom m = case coerce m of FakeIO f -> case f newWorld of (# _, a #) -> a
  where
    newWorld = FakeWorld
      { time = 0
      , refs = []
      , heap = FakeHeap {heapNext = 0, heapMem = []}
      }

returnIOAxiom
  :: forall io a
  . Coercible FakeIO io
  => a -> io a
returnIOAxiom a = coerce (FakeIO $ \s -> (# nextWorld s, a #))

bindIOAxiom
  :: forall io a b
  . Coercible FakeIO io
  => io a -> (a -> io b) -> io b
bindIOAxiom m k = coerce (bindFakeIO (coerce m :: FakeIO a) (\x -> coerce (k x) :: FakeIO b))
  where
    bindFakeIO :: FakeIO a -> (a -> FakeIO b) -> FakeIO b
    bindFakeIO (FakeIO f) g = FakeIO $ \s -> case f s of
      (# s', a #) -> case g a of
        FakeIO n -> n s'

newIORefAxiom
  :: forall a io ioref
  . Coercible FakeIO io
  => Coercible FakeIORef ioref
  => a -> io (ioref a)
newIORefAxiom a =
  let f :: FakeWorld -> (# FakeWorld, FakeIORef a #)
      f s = let s' = s { time = time s + 1, refs = append (refs s) [unsafeCoerce a] }
                ref = FakeIORef { refID = time s, value = a }
            in (# s', ref #)
      m :: io (FakeIORef a)
      m = coerce (FakeIO f)
  in coerce m

readIORefAxiom
  :: forall a io ioref
  . Coercible FakeIO io
  => Coercible FakeIORef ioref
  => ioref a -> io a
readIORefAxiom ref =
  let ref' :: FakeIORef a
      ref' = coerce ref
      f :: FakeWorld -> (# FakeWorld, a #)
      f s = let FakeIORef { refID } = ref'
                idx = fromIntegral (Pantomime.toInteger refID)
                val = unsafeCoerce (refs s !! idx)
            in (# nextWorld s, val #)
  in coerce (FakeIO f)

writeIORefAxiom
  :: forall a io ioref
  . Coercible FakeIO io
  => Coercible FakeIORef ioref
  => ioref a -> a -> io ()
writeIORefAxiom ref a =
  let ref' :: FakeIORef a
      ref' = coerce ref
      f :: FakeWorld -> (# FakeWorld, () #)
      f s = let FakeIORef { refID } = ref'
                idx = fromIntegral (Pantomime.toInteger refID)
                s' = s { refs = updateAt idx (unsafeCoerce a) (refs s) }
            in (# nextWorld s', () #)
  in coerce (FakeIO f)

append :: [a] -> [a] -> [a]
append [] ys = ys
append (x : xs) ys = x : append xs ys

updateAt :: Int -> a -> [a] -> [a]
updateAt 0 y (_ : xs) = y : xs
updateAt n y (x : xs) = x : updateAt (n - 1) y xs
updateAt _ _ [] = []
