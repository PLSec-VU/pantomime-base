{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module ContainersTest (spec) where

import Common
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Pantomime.BuiltIn qualified as Pantomime

{-# ANN mapMemberEmpty (Theory axioms) #-}
mapMemberEmpty :: Int -> Pantomime.Bool
mapMemberEmpty k = Pantomime.boolean $ not (Map.member k (Map.empty :: Map.Map Int Int))

{-# ANN mapMemberSingleton (Theory axioms) #-}
mapMemberSingleton :: Int -> Int -> Pantomime.Bool
mapMemberSingleton k v = Pantomime.boolean $ Map.member k (Map.singleton k v)

{-# ANN mapLookupSingleton (Theory axioms) #-}
mapLookupSingleton :: Int -> Int -> Pantomime.Bool
mapLookupSingleton k v = Pantomime.boolean $ Map.lookup k (Map.singleton k v) == Just v

{-# ANN mapMemberInsert (Theory axioms) #-}
mapMemberInsert :: Int -> Int -> Pantomime.Bool
mapMemberInsert k v = Pantomime.boolean $ Map.member k (Map.insert k v Map.empty)

{-# ANN mapLookupInsert (Theory axioms) #-}
mapLookupInsert :: Int -> Int -> Pantomime.Bool
mapLookupInsert k v = Pantomime.boolean $ Map.lookup k (Map.insert k v Map.empty) == Just v

{-# ANN mapDeleteSelf (Theory axioms) #-}
mapDeleteSelf :: Int -> Pantomime.Bool
mapDeleteSelf k = Pantomime.boolean $ Map.null (Map.delete k (Map.singleton k (0 :: Int)))

{-# ANN mapLookupDifferentKey (Theory axioms) #-}
mapLookupDifferentKey :: Int -> Int -> Int -> Pantomime.Bool
mapLookupDifferentKey k1 k2 v = Pantomime.boolean $
  (k1 /= k2) `implies` (Map.lookup k1 (Map.singleton k2 v) == Nothing)
  where
    implies False _ = True
    implies True  x = x

{-# ANN setMemberEmpty (Theory axioms) #-}
setMemberEmpty :: Int -> Pantomime.Bool
setMemberEmpty k = Pantomime.boolean $ not (Set.member k Set.empty)

{-# ANN setMemberSingleton (Theory axioms) #-}
setMemberSingleton :: Int -> Pantomime.Bool
setMemberSingleton k = Pantomime.boolean $ Set.member k (Set.singleton k)

{-# ANN setMemberInsert (Theory axioms) #-}
setMemberInsert :: Int -> Pantomime.Bool
setMemberInsert k = Pantomime.boolean $ Set.member k (Set.insert k Set.empty)

{-# ANN setDeleteSelf (Theory axioms) #-}
setDeleteSelf :: Int -> Pantomime.Bool
setDeleteSelf k = Pantomime.boolean $ not (Set.member k (Set.delete k (Set.singleton k)))

spec :: Spec
spec = describe "containers (Data.Map.Strict + Data.Set)" $ do
  describe "Data.Map.Strict" $ do
    it "member k empty == False" $ $(pantomime 'mapMemberEmpty) `shouldBe` Nothing
    it "member k (singleton k v) == True" $ $(pantomime 'mapMemberSingleton) `shouldBe` Nothing
    it "lookup k (singleton k v) == Just v" $ $(pantomime 'mapLookupSingleton) `shouldBe` Nothing
    it "member k (insert k v empty) == True" $ $(pantomime 'mapMemberInsert) `shouldBe` Nothing
    it "lookup k (insert k v empty) == Just v" $ $(pantomime 'mapLookupInsert) `shouldBe` Nothing
    it "null (delete k (singleton k v)) == True" $ $(pantomime 'mapDeleteSelf) `shouldBe` Nothing
    it "k1 /= k2 implies lookup k1 (singleton k2 v) == Nothing" $ $(pantomime 'mapLookupDifferentKey) `shouldBe` Nothing
  describe "Data.Set" $ do
    it "member k empty == False" $ $(pantomime 'setMemberEmpty) `shouldBe` Nothing
    it "member k (singleton k) == True" $ $(pantomime 'setMemberSingleton) `shouldBe` Nothing
    it "member k (insert k empty) == True" $ $(pantomime 'setMemberInsert) `shouldBe` Nothing
    it "not (member k (delete k (singleton k))) == True" $ $(pantomime 'setDeleteSelf) `shouldBe` Nothing
