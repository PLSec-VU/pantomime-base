{-# OPTIONS_GHC -Wno-orphans -Wno-unused-top-binds #-}
module TestEncodeAnn (spec) where
import Common
import Data.ByteString (ByteString)
import Data.ByteString.Base64 qualified as B64
import Data.ByteString.Base64.Internal (withBS, mkBS, peek8, poke8, withForeignPtrN, plusPtrN)
import Data.Word (Word8)
import Foreign.ForeignPtr (ForeignPtr)
import Pantomime.BuiltIn qualified as Pantomime
import Pantomime.Ptr (FakeForeignPtr (..), FakePtr (..))
import System.IO.Unsafe (unsafePerformIO)
import Unsafe.Coerce (unsafeCoerce)
import GHC.Base (Int (..))

-- | Build a ByteString of length n backed by a symbolic heap buffer.
mkSymbolicBS :: [Word8] -> Int -> ByteString
mkSymbolicBS bytes n =
  unsafePerformIO $ do
    let fp = unsafeCoerce (FakeForeignPtr (Pantomime.fromInt# 3#) (case n of I# n# -> Pantomime.fromInt# n#)) :: ForeignPtr Word8
    withForeignPtrN fp $ \p -> do
      pokeBytes p bytes
      return (mkBS fp n)
  where
    pokeBytes _ [] = return ()
    pokeBytes p (b : bs) = poke8 p b >> pokeBytes (p `plusPtrN` 1) bs

peekBSBytes :: ByteString -> Int -> [Word8]
peekBSBytes bs n =
  unsafePerformIO $
    withBS bs $ \p _ ->
      return (peekBytes p n)
  where
    peekBytes _ 0 = return []
    peekBytes p k = do
      b <- peek8 p
      rest <- peekBytes (p `plusPtrN` 1) (k - 1)
      return (b : rest)

encodeSingleBytePads :: Word8 -> Pantomime.Bool
encodeSingleBytePads b =
  let output = peekBSBytes (B64.encode (mkSymbolicBS [b] 1)) 4
  in case output of
       [_, _, c2, c3] -> Pantomime.boolean (c2 == 0x3d) Pantomime.&& Pantomime.boolean (c3 == 0x3d)
       _ -> Pantomime.false

encodeLengthIs4 :: Word8 -> Pantomime.Bool
encodeLengthIs4 b = Pantomime.boolean $
  withBS (B64.encode (mkSymbolicBS [b] 1)) (\_ len -> return (len == 4))

encodeLength2Is4 :: Word8 -> Word8 -> Pantomime.Bool
encodeLength2Is4 a b = Pantomime.boolean $
  withBS (B64.encode (mkSymbolicBS [a, b] 2)) (\_ len -> return (len == 4))

encodeLength3Is4 :: Word8 -> Word8 -> Word8 -> Pantomime.Bool
encodeLength3Is4 a b c = Pantomime.boolean $
  withBS (B64.encode (mkSymbolicBS [a, b, c] 3)) (\_ len -> return (len == 4))

-- NOTE: These tests have a regression in the current version of the bytestring
-- axioms; annotations are disabled until it is fixed.
spec :: Spec
spec = describe "real encode" $ do
  it "B64.encode (1 byte) pads last 2 chars with '='" todo
  it "B64.encode (1 byte) has length 4" todo
  it "B64.encode (2 bytes) has length 4" todo
  it "B64.encode (3 bytes) has length 4" todo
