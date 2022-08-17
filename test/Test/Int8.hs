{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Test.Int8 (testTree) where

import Data.Bool.Prim (Bool#, toBool#)
import Data.Int.Prim

import GHC.Int (Int8)

import Hedgehog (Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Test.Tasty (TestTree, testGroup)
import Test.Compat (prop)

--------------------------------------------------------------------------------

testTree :: TestTree
testTree =
  testGroup
    "Int8"
    [ testGroup
        "Arithmetic"
        [ prop "addInt8#" $ arithmetic addInt8# (+)
        , prop "subInt8#" $ arithmetic subInt8# (-)
        , prop "mulInt8#" $ arithmetic mulInt8# (*)
        ]
    , testGroup
        "Comparison"
        [ prop "gtInt8#" $ comparison gtInt8# (>)
        , prop "geInt8#" $ comparison geInt8# (>=)
        , prop "eqInt8#" $ comparison eqInt8# (==)
        , prop "neInt8#" $ comparison neInt8# (/=)
        , prop "ltInt8#" $ comparison ltInt8# (<)
        , prop "leInt8#" $ comparison leInt8# (<=)
        ]
    ]

arithmetic :: (Int8# -> Int8# -> Int8#) -> (Int8 -> Int8 -> Int8) -> Property
arithmetic op# op = property $ do
  x@(toInt8# -> x#) <- forAll (Gen.int8 Range.constantBounded)
  y@(toInt8# -> y#) <- forAll (Gen.int8 Range.constantBounded)
  op x y === fromInt8# (op# x# y#)

comparison :: (Int8# -> Int8# -> Bool#) -> (Int8 -> Int8 -> Bool) -> Property
comparison op# op = property $ do
  x@(toInt8# -> x#) <- forAll (Gen.int8 Range.constantBounded)
  y@(toInt8# -> y#) <- forAll (Gen.int8 Range.constantBounded)
  op x y === toBool# (op# x# y#)