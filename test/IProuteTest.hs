{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module IProuteTest (spec) where

import Common
import Data.Bits
import Data.Word (Word32, Word64)
import Pantomime.BuiltIn qualified as Pantomime

-- Inlined from Data.IP.Addr / Data.IP.Range / Data.IP.Mask / Data.IP.Op
-- (the pure arithmetic core of the iproute library, without socket/parser deps)

newtype IPv4 = IP4 Word32 deriving (Eq, Ord)

newtype IPv6 = IP6 (Word32, Word32, Word32, Word32) deriving (Eq, Ord)

data AddrRange a = AddrRange
    { addr :: !a
    , mask :: !a
    , mlen :: !Int
    } deriving (Eq, Ord)

maskedIPv4 :: IPv4 -> IPv4 -> IPv4
maskedIPv4 (IP4 a) (IP4 m) = IP4 (a .&. m)

maskedIPv6 :: IPv6 -> IPv6 -> IPv6
maskedIPv6 (IP6 (a1, a2, a3, a4)) (IP6 (m1, m2, m3, m4)) =
    IP6 (a1 .&. m1, a2 .&. m2, a3 .&. m3, a4 .&. m4)

maskIPv4 :: Int -> IPv4
maskIPv4 len = IP4 $ complement $ (0xffffffff :: Word32) `shift` (-len)

toIP6Addr :: (Word64, Word64) -> (Word32, Word32, Word32, Word32)
toIP6Addr (h, l) =
    ( fromIntegral $ (h `shiftR` 32) .&. m
    , fromIntegral $ h .&. m
    , fromIntegral $ (l `shiftR` 32) .&. m
    , fromIntegral $ l .&. m
    )
  where
    m = 0xffffffff

shiftR128 :: (Word64, Word64) -> Int -> (Word64, Word64)
shiftR128 (h, l) i =
    (h `shiftR` i, (l `shiftR` i) .|. h `shift` (64 - i))

shiftL128 :: (Word64, Word64) -> Int -> (Word64, Word64)
shiftL128 (h, l) i =
    ((h `shiftL` i) .|. (l `shift` (i - 64)), l `shiftL` i)

shift128 :: (Word64, Word64) -> Int -> (Word64, Word64)
shift128 x i
    | i < 0    = x `shiftR128` (-i)
    | i > 0    = x `shiftL128` i
    | otherwise = x

maskIPv6 :: Int -> IPv6
maskIPv6 len =
    IP6 $
        toIP6Addr $
            bimapTup complement $
                (0xffffffffffffffff, 0xffffffffffffffff) `shift128` (-len)
  where
    bimapTup f (x, y) = (f x, f y)

class Eq a => Addr a where
    masked    :: a -> a -> a
    intToMask :: Int -> a

instance Addr IPv4 where
    masked    = maskedIPv4
    intToMask = maskIPv4

instance Addr IPv6 where
    masked    = maskedIPv6
    intToMask = maskIPv6

isMatchedTo :: Addr a => a -> AddrRange a -> Bool
isMatchedTo a r = a `masked` mask r == addr r

(>:>) :: Addr a => AddrRange a -> AddrRange a -> Bool
(>:>) a b = mlen a <= mlen b && (addr b `masked` mask a) == addr a

makeAddrRange :: Addr a => a -> Int -> AddrRange a
makeAddrRange ad len = AddrRange adr msk len
  where
    msk = intToMask len
    adr = ad `masked` msk

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
    let a = IP4 w
    in  a `isMatchedTo` makeAddrRange a len

-- | Subnet containment is reflexive.
{-# ANN subnetReflexive (Theory axioms) #-}
subnetReflexive :: Word32 -> Int -> Pantomime.Bool
subnetReflexive w len = Pantomime.boolean $
    let r = makeAddrRange (IP4 w) len
    in  r >:> r

-- | Subnet containment is transitive (for valid IPv4 mask lengths 0–32).
{-# ANN subnetTransitive (Theory axioms) #-}
subnetTransitive :: Word32 -> Int -> Word32 -> Int -> Word32 -> Int -> Pantomime.Bool
subnetTransitive w1 l1 w2 l2 w3 l3 = Pantomime.boolean $
    let validLens = 0 <= l1 && l1 <= 32 && 0 <= l2 && l2 <= 32 && 0 <= l3 && l3 <= 32
        r1 = makeAddrRange (IP4 w1) l1
        r2 = makeAddrRange (IP4 w2) l2
        r3 = makeAddrRange (IP4 w3) l3
    in  not validLens || not (r1 >:> r2 && r2 >:> r3) || r1 >:> r3

addrInOwnRangeIPv6 :: Word32 -> Word32 -> Word32 -> Word32 -> Int -> Pantomime.Bool
addrInOwnRangeIPv6 w1 w2 w3 w4 len = Pantomime.boolean $
    let a = IP6 (w1, w2, w3, w4)
    in  a `isMatchedTo` makeAddrRange a len

subnetReflexiveIPv6 :: Word32 -> Word32 -> Word32 -> Word32 -> Int -> Pantomime.Bool
subnetReflexiveIPv6 w1 w2 w3 w4 len = Pantomime.boolean $
    let r = makeAddrRange (IP6 (w1, w2, w3, w4)) len
    in  r >:> r

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
