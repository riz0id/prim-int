{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Int (testTree) where

import Hedgehog (Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testPropertyNamed)

import Data.Bool.Unlifted (Bool#, toBool#)
import Data.Int.Prim
import GHC.Int (Int (I#))

--------------------------------------------------------------------------------

testTree :: TestTree
testTree =
  testGroup
    "Int"
    [ testGroup
        "Arithmetic"
        [ testPropertyNamed "addInt#" "addInt#" $ arithmetic addInt# (+)
        , testPropertyNamed "subInt#" "subInt#" $ arithmetic subInt# (-)
        , testPropertyNamed "mulInt#" "mulInt#" $ arithmetic mulInt# (*)
        ]
    , testGroup
        "Comparison"
        [ testPropertyNamed "gtInt#" "gtInt#" $ comparison gtInt# (>)
        , testPropertyNamed "geInt#" "geInt#" $ comparison geInt# (>=)
        , testPropertyNamed "eqInt#" "eqInt#" $ comparison eqInt# (==)
        , testPropertyNamed "neInt#" "neInt#" $ comparison neInt# (/=)
        , testPropertyNamed "ltInt#" "ltInt#" $ comparison ltInt# (<)
        , testPropertyNamed "leInt#" "leInt#" $ comparison leInt# (<=)
        ]
    ]

arithmetic :: (Int# -> Int# -> Int#) -> (Int -> Int -> Int) -> Property
arithmetic op# op = property $ do
  x@(I# x#) <- forAll (Gen.int Range.constantBounded)
  y@(I# y#) <- forAll (Gen.int Range.constantBounded)
  op x y === I# (op# x# y#)

comparison :: (Int# -> Int# -> Bool#) -> (Int -> Int -> Bool) -> Property
comparison op# op = property $ do
  x@(I# x#) <- forAll (Gen.int Range.constantBounded)
  y@(I# y#) <- forAll (Gen.int Range.constantBounded)
  op x y === toBool# (op# x# y#)