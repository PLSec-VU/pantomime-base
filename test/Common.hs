{-# OPTIONS_GHC -Wno-orphans #-}

module Common
  ( checkInvalid
  , todo
  , axioms
  , module Test.Hspec
  , module Pantomime
  , module GHC.Exts
  , module GHC.Int
  , module GHC.Word
  ) where

import Test.Hspec
import Test.Hspec.Expectations (expectationFailure)

import Pantomime (Theory (..), pantomime)
import Pantomime.Base (axioms)
import Pantomime.BuiltIn qualified as Pantomime

import GHC.Exts
import GHC.Int
import GHC.Word

-- | Placeholder expectation for tests whose pantomime TH splice is not yet active.
todo :: Expectation
todo = pure ()

-- | Assert that a counterexample was found and print it.
checkInvalid :: Maybe String -> Expectation
checkInvalid = \case
  Just ce -> do
    putStrLn ""
    putStrLn "Counterexample found:"
    putStrLn ce
    putStrLn ""
  Nothing -> expectationFailure "Expected a counterexample but assertion was valid"
