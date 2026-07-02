{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module IProuteTest (spec) where

import Common
import Data.Bits
import Data.IP (IPv4, AddrRange, Addr (..), makeAddrRange, isMatchedTo, (>:>), toIPv4w)
import Data.Word (Word32)
import Pantomime.BuiltIn qualified as Pantomime

-- | Internal helper from Data.IP.Addr, reproduced verbatim (not part of public API).
fixByteOrder :: Word32 -> Word32
fixByteOrder s = d1 .|. d2 .|. d3 .|. d4
  where
    d1 = shiftL s 24
    d2 = shiftL s 8 .&. 0x00ff0000
    d3 = shiftR s 8 .&. 0x0000ff00
    d4 = shiftR s 24 .&. 0x000000ff

-- | Byte-swapping is its own inverse.
{-# ANN fixByteOrderInvolution (Theory axioms) #-}
fixByteOrderInvolution :: Word32 -> Pantomime.Bool
fixByteOrderInvolution w = Pantomime.boolean $ fixByteOrder (fixByteOrder w) == w

-- | Any IPv4 address is contained in the subnet it generates.
{-# ANN addrInOwnRange (Theory axioms) #-}
addrInOwnRange :: Word32 -> Int -> Pantomime.Bool
addrInOwnRange w len = Pantomime.boolean $
    let a = toIPv4w w
    in  a `isMatchedTo` makeAddrRange a len

-- | Subnet containment is reflexive.
{-# ANN subnetReflexive (Theory axioms) #-}
subnetReflexive :: Word32 -> Int -> Pantomime.Bool
subnetReflexive w len = Pantomime.boolean $
    let r = makeAddrRange (toIPv4w w) len
    in  r >:> r

-- | Subnet containment is transitive (for valid IPv4 mask lengths 0-32).
{-# ANN subnetTransitive (Theory axioms) #-}
subnetTransitive :: Word32 -> Int -> Word32 -> Int -> Word32 -> Int -> Pantomime.Bool
subnetTransitive w1 l1 w2 l2 w3 l3 = Pantomime.boolean $
    let validLens = 0 <= l1 && l1 <= 32 && 0 <= l2 && l2 <= 32 && 0 <= l3 && l3 <= 32
        r1 = makeAddrRange (toIPv4w w1) l1
        r2 = makeAddrRange (toIPv4w w2) l2
        r3 = makeAddrRange (toIPv4w w3) l3
    in  not validLens || not (r1 >:> r2 && r2 >:> r3) || r1 >:> r3

spec :: Spec
spec = describe "iproute address arithmetic" $ do
  describe "IPv4" $ do
    it "fixByteOrder is an involution" $
      $(pantomime 'fixByteOrderInvolution) `shouldBe` Nothing
    it "makeAddrRange always contains its own address" $
      $(pantomime 'addrInOwnRange) `shouldBe` Nothing
    it "subnet containment is reflexive" $
      $(pantomime 'subnetReflexive) `shouldBe` Nothing
    it "subnet containment is transitive" $
      $(pantomime 'subnetTransitive) `shouldBe` Nothing
