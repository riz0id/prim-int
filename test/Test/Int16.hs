{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Int16 (testTree) where

import Hedgehog (Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testPropertyNamed)

import Data.Bool.Unlifted (Bool#, toBool#)
import Data.Int.Prim
import GHC.Int (Int16 (I16#))

--------------------------------------------------------------------------------

testTree :: TestTree
testTree =
  testGroup
    "Int16"
    [ testGroup
        "Arithmetic"
        [ testPropertyNamed "addInt16#" "addInt16#" $ arithmetic addInt16# (+)
        , testPropertyNamed "subInt16#" "subInt16#" $ arithmetic subInt16# (-)
        , testPropertyNamed "mulInt16#" "mulInt16#" $ arithmetic mulInt16# (*)
        ]
    , testGroup
        "Comparison"
        [ testPropertyNamed "gtInt16#" "gtInt16#" $ comparison gtInt16# (>)
        , testPropertyNamed "geInt16#" "geInt16#" $ comparison geInt16# (>=)
        , testPropertyNamed "eqInt16#" "eqInt16#" $ comparison eqInt16# (==)
        , testPropertyNamed "neInt16#" "neInt16#" $ comparison neInt16# (/=)
        , testPropertyNamed "ltInt16#" "ltInt16#" $ comparison ltInt16# (<)
        , testPropertyNamed "leInt16#" "leInt16#" $ comparison leInt16# (<=)
        ]
    ]

arithmetic :: (Int16# -> Int16# -> Int16#) -> (Int16 -> Int16 -> Int16) -> Property
arithmetic op# op = property $ do
  x@(I16# x#) <- forAll (Gen.int16 Range.constantBounded)
  y@(I16# y#) <- forAll (Gen.int16 Range.constantBounded)
  op x y === I16# (op# x# y#)

comparison :: (Int16# -> Int16# -> Bool#) -> (Int16 -> Int16 -> Bool) -> Property
comparison op# op = property $ do
  x@(I16# x#) <- forAll (Gen.int16 Range.constantBounded)
  y@(I16# y#) <- forAll (Gen.int16 Range.constantBounded)
  op x y === toBool# (op# x# y#)