{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Test.Int16 (testTree) where

import Data.Bool.Prim (Bool#, toBool)
import Data.Int.Prim

import GHC.Int (Int16)

import Hedgehog (Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Test.Tasty (TestTree, testGroup)
import Test.Compat (prop)

--------------------------------------------------------------------------------

testTree :: TestTree
testTree =
  testGroup
    "Int16"
    [ testGroup
        "Arithmetic"
        [ prop "addInt16#" $ arithmetic addInt16# (+)
        , prop "subInt16#" $ arithmetic subInt16# (-)
        , prop "mulInt16#" $ arithmetic mulInt16# (*)
        ]
    , testGroup
        "Comparison"
        [ prop "gtInt16#" $ comparison gtInt16# (>)
        , prop "geInt16#" $ comparison geInt16# (>=)
        , prop "eqInt16#" $ comparison eqInt16# (==)
        , prop "neInt16#" $ comparison neInt16# (/=)
        , prop "ltInt16#" $ comparison ltInt16# (<)
        , prop "leInt16#" $ comparison leInt16# (<=)
        ]
    ]

arithmetic :: (Int16# -> Int16# -> Int16#) -> (Int16 -> Int16 -> Int16) -> Property
arithmetic op# op = property $ do
  x@(toInt16# -> x#) <- forAll (Gen.int16 Range.constantBounded)
  y@(toInt16# -> y#) <- forAll (Gen.int16 Range.constantBounded)
  op x y === fromInt16# (op# x# y#)

comparison :: (Int16# -> Int16# -> Bool#) -> (Int16 -> Int16 -> Bool) -> Property
comparison op# op = property $ do
  x@(toInt16# -> x#) <- forAll (Gen.int16 Range.constantBounded)
  y@(toInt16# -> y#) <- forAll (Gen.int16 Range.constantBounded)
  op x y === toBool (op# x# y#)