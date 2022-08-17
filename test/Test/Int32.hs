{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Int32 (testTree) where

import Hedgehog (Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testPropertyNamed)

import Data.Bool.Unlifted (Bool#, toBool#)
import Data.Int.Prim
import GHC.Int (Int32 (I32#))

--------------------------------------------------------------------------------

testTree :: TestTree
testTree =
  testGroup
    "Int32"
    [ testGroup
        "Arithmetic"
        [ testPropertyNamed "addInt32#" "addInt32#" $ arithmetic addInt32# (+)
        , testPropertyNamed "subInt32#" "subInt32#" $ arithmetic subInt32# (-)
        , testPropertyNamed "mulInt32#" "mulInt32#" $ arithmetic mulInt32# (*)
        ]
    , testGroup
        "Comparison"
        [ testPropertyNamed "gtInt32#" "gtInt32#" $ comparison gtInt32# (>)
        , testPropertyNamed "geInt32#" "geInt32#" $ comparison geInt32# (>=)
        , testPropertyNamed "eqInt32#" "eqInt32#" $ comparison eqInt32# (==)
        , testPropertyNamed "neInt32#" "neInt32#" $ comparison neInt32# (/=)
        , testPropertyNamed "ltInt32#" "ltInt32#" $ comparison ltInt32# (<)
        , testPropertyNamed "leInt32#" "leInt32#" $ comparison leInt32# (<=)
        ]
    ]

arithmetic :: (Int32# -> Int32# -> Int32#) -> (Int32 -> Int32 -> Int32) -> Property
arithmetic op# op = property $ do
  x@(I32# x#) <- forAll (Gen.int32 Range.constantBounded)
  y@(I32# y#) <- forAll (Gen.int32 Range.constantBounded)
  op x y === I32# (op# x# y#)

comparison :: (Int32# -> Int32# -> Bool#) -> (Int32 -> Int32 -> Bool) -> Property
comparison op# op = property $ do
  x@(I32# x#) <- forAll (Gen.int32 Range.constantBounded)
  y@(I32# y#) <- forAll (Gen.int32 Range.constantBounded)
  op x y === toBool# (op# x# y#)