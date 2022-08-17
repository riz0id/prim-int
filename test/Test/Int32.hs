{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Test.Int32 (testTree) where

import Data.Bool.Prim (Bool#, toBool#)
import Data.Int.Prim

import GHC.Int (Int32)

import Hedgehog (Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Test.Tasty (TestTree, testGroup)
import Test.Compat (prop)

--------------------------------------------------------------------------------

testTree :: TestTree
testTree =
  testGroup
    "Int32"
    [ testGroup
        "Arithmetic"
        [ prop "addInt32#" $ arithmetic addInt32# (+)
        , prop "subInt32#" $ arithmetic subInt32# (-)
        , prop "mulInt32#" $ arithmetic mulInt32# (*)
        ]
    , testGroup
        "Comparison"
        [ prop "gtInt32#" $ comparison gtInt32# (>)
        , prop "geInt32#" $ comparison geInt32# (>=)
        , prop "eqInt32#" $ comparison eqInt32# (==)
        , prop "neInt32#" $ comparison neInt32# (/=)
        , prop "ltInt32#" $ comparison ltInt32# (<)
        , prop "leInt32#" $ comparison leInt32# (<=)
        ]
    ]

arithmetic :: (Int32# -> Int32# -> Int32#) -> (Int32 -> Int32 -> Int32) -> Property
arithmetic op# op = property $ do
  x@(toInt32# -> x#) <- forAll (Gen.int32 Range.constantBounded)
  y@(toInt32# -> y#) <- forAll (Gen.int32 Range.constantBounded)
  op x y === fromInt32# (op# x# y#)

comparison :: (Int32# -> Int32# -> Bool#) -> (Int32 -> Int32 -> Bool) -> Property
comparison op# op = property $ do
  x@(toInt32# -> x#) <- forAll (Gen.int32 Range.constantBounded)
  y@(toInt32# -> y#) <- forAll (Gen.int32 Range.constantBounded)
  op x y === toBool# (op# x# y#)