
module Main (main) where

import Test.Tasty (TestTree, defaultMain, localOption, testGroup)
import Test.Tasty.Hedgehog (HedgehogTestLimit (HedgehogTestLimit))

import qualified Test.Int
import qualified Test.Int8
import qualified Test.Int16
import qualified Test.Int32

--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain (localOption (HedgehogTestLimit (Just 100000)) testTree)

testTree :: TestTree
testTree = 
  testGroup 
    "Test"
    [ Test.Int.testTree
    , Test.Int8.testTree
    , Test.Int16.testTree
    , Test.Int32.testTree
    ]