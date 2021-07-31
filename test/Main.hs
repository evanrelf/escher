{-# OPTIONS_GHC -Wno-unused-imports #-}

module Main (main) where

import Text.Show.Pretty (ppShow)

import qualified Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.Golden as Tasty.Golden
import qualified Test.Tasty.HUnit as Tasty.HUnit
import qualified Test.Tasty.Hedgehog as Tasty.Hedgehog


main :: IO ()
main = Tasty.defaultMain $ Tasty.testGroup "tests"
  [ test_foo
  ]


test_foo :: Tasty.TestTree
test_foo = Tasty.testGroup "foo"
  [
  ]
