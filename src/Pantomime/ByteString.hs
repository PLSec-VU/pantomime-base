{-# LANGUAGE MagicHash #-}

module Pantomime.ByteString
  ( byteStringAxioms,
    ByteStringR,
  )
where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import GHC.Base (Int (..))
import GHC.Word (Word8 (..))
import GHC.Exts (IsList (..))
import Pantomime (PluginAxioms (..))
import Pantomime.BuiltIn qualified as Pantomime
import Unsafe.Coerce (unsafeCoerce)

-- | Symbolic representation of a strict 'ByteString': a symbolic array
-- mapping byte index to byte value.
type ByteStringR = Pantomime.Array Pantomime.Integer (Pantomime.BitVec 8)

byteStringAxioms :: PluginAxioms
byteStringAxioms =
  PluginAxioms
    { typeAxioms =
        fromList
          [ (''ByteString, ''ByteStringR)
          ],
      termAxioms =
        [ ('BS.empty, 'bsEmpty),
          ('BS.singleton, 'bsSingleton),
          ('BS.index, 'bsIndex),
          ('BS.head, 'bsHead)
        ]
    }

-- =============================================================================
-- ByteString interpretation functions
-- =============================================================================

bsEmpty :: ByteString
bsEmpty =
  let zero = 0 :: Pantomime.BitVec 8
   in unsafeCoerce $ Pantomime.aconst @Pantomime.Integer @(Pantomime.BitVec 8) zero

bsSingleton :: Word8 -> ByteString
bsSingleton (W8# w#) =
  let zeroBv = 0 :: Pantomime.BitVec 8
      zeroIx = 0 :: Pantomime.Integer
      arr = Pantomime.aconst @Pantomime.Integer @(Pantomime.BitVec 8) zeroBv
   in unsafeCoerce $ Pantomime.astore arr zeroIx (Pantomime.fromWord8# w#)

bsIndex :: ByteString -> Int -> Word8
bsIndex bs (I# i#) =
  let arr = unsafeCoerce bs :: ByteStringR
      idx = Pantomime.bvu2i $ Pantomime.fromInt# i#
      val = Pantomime.aselect arr idx
   in W8# (Pantomime.toWord8# val)

bsHead :: ByteString -> Word8
bsHead bs =
  let arr = unsafeCoerce bs :: ByteStringR
      val = Pantomime.aselect arr 0
   in W8# (Pantomime.toWord8# val)
