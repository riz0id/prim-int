{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Test.Int (testTree) where

import Data.Bool.Prim (Bool#, toBool#)
import Data.Int.Prim

import GHC.Int (Int (I#))

import Hedgehog (Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Test.Tasty (TestTree, testGroup)
import Test.Compat (prop)

--------------------------------------------------------------------------------

testTree :: TestTree
testTree =
  testGroup
    "Int"
    [ testGroup
        "Arithmetic"
        [ prop "addInt#" $ arithmetic addInt# (+)
        , prop "subInt#" $ arithmetic subInt# (-)
        , prop "mulInt#" $ arithmetic mulInt# (*)
        ]
    , testGroup
        "Comparison"
        [ prop "gtInt#" $ comparison gtInt# (>)
        , prop "geInt#" $ comparison geInt# (>=)
        , prop "eqInt#" $ comparison eqInt# (==)
        , prop "neInt#" $ comparison neInt# (/=)
        , prop "ltInt#" $ comparison ltInt# (<)
        , prop "leInt#" $ comparison leInt# (<=)
        ]
    ]

arithmetic :: (Int# -> Int# -> Int#) -> (Int -> Int -> Int) -> Property
arithmetic op# op = property $ do
  x@(toInt# -> x#) <- forAll (Gen.int Range.constantBounded)
  y@(toInt# -> y#) <- forAll (Gen.int Range.constantBounded)
  op x y === I# (op# x# y#)

comparison :: (Int# -> Int# -> Bool#) -> (Int -> Int -> Bool) -> Property
comparison op# op = property $ do
  x@(toInt# -> x#) <- forAll (Gen.int Range.constantBounded)
  y@(toInt# -> y#) <- forAll (Gen.int Range.constantBounded)
  op x y === toBool# (op# x# y#)