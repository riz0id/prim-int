{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Int8 (testTree) where

import Hedgehog (Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testPropertyNamed)

import Data.Bool.Unlifted (Bool#, toBool#)
import Data.Int.Prim
import GHC.Int (Int8 (I8#))

--------------------------------------------------------------------------------

testTree :: TestTree
testTree =
  testGroup
    "Int8"
    [ testGroup
        "Arithmetic"
        [ testPropertyNamed "addInt8#" "addInt8#" $ arithmetic addInt8# (+)
        , testPropertyNamed "subInt8#" "subInt8#" $ arithmetic subInt8# (-)
        , testPropertyNamed "mulInt8#" "mulInt8#" $ arithmetic mulInt8# (*)
        ]
    , testGroup
        "Comparison"
        [ testPropertyNamed "gtInt8#" "gtInt8#" $ comparison gtInt8# (>)
        , testPropertyNamed "geInt8#" "geInt8#" $ comparison geInt8# (>=)
        , testPropertyNamed "eqInt8#" "eqInt8#" $ comparison eqInt8# (==)
        , testPropertyNamed "neInt8#" "neInt8#" $ comparison neInt8# (/=)
        , testPropertyNamed "ltInt8#" "ltInt8#" $ comparison ltInt8# (<)
        , testPropertyNamed "leInt8#" "leInt8#" $ comparison leInt8# (<=)
        ]
    ]

arithmetic :: (Int8# -> Int8# -> Int8#) -> (Int8 -> Int8 -> Int8) -> Property
arithmetic op# op = property $ do
  x@(I8# x#) <- forAll (Gen.int8 Range.constantBounded)
  y@(I8# y#) <- forAll (Gen.int8 Range.constantBounded)
  op x y === I8# (op# x# y#)

comparison :: (Int8# -> Int8# -> Bool#) -> (Int8 -> Int8 -> Bool) -> Property
comparison op# op = property $ do
  x@(I8# x#) <- forAll (Gen.int8 Range.constantBounded)
  y@(I8# y#) <- forAll (Gen.int8 Range.constantBounded)
  op x y === toBool# (op# x# y#)