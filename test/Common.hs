{-# OPTIONS_GHC -Wno-orphans #-}

module Common
  ( checkInvalid
  , todo
  , axioms
  , ioAxioms
  , ptrAxioms
  , module Test.Hspec
  , module Pantomime
  , module GHC.Exts
  , module GHC.Int
  , module GHC.Word
  ) where

import Test.Hspec
import Test.Hspec.Expectations (expectationFailure)

import Pantomime.Base (axioms)
import Pantomime (Theory (..), pantomime)
import Pantomime.IO (ioAxioms)
import Pantomime.Ptr (ptrAxioms)
import Pantomime.BuiltIn qualified as Pantomime

import GHC.Exts
import GHC.Int
import GHC.Word

-- | Placeholder expectation for tests whose pantomime TH splice is not yet active.
todo :: Expectation
todo = pure ()

-- | Assert that a counterexample was found and print it.
checkInvalid :: Show a => Maybe a -> Expectation
checkInvalid = \case
  Just ce -> do
    putStrLn ""
    putStrLn "Counterexample found:"
    print ce
    putStrLn ""
  Nothing -> expectationFailure "Expected a counterexample but assertion was valid"
