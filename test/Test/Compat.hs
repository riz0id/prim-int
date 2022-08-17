{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Compat (prop) where

import Hedgehog (Property)
import Test.Tasty (TestName, TestTree)

#if MIN_VERSION_tasty_hedgehog(1,2,0)

import Data.String (fromString)
import Test.Tasty.Hedgehog (testPropertyNamed)

#else

import Test.Tasty.Hedgehog (testProperty)

#endif

--------------------------------------------------------------------------------

#if MIN_VERSION_tasty_hedgehog(1,2,0)

prop :: TestName -> Property -> TestTree
prop name = testPropertyNamed name (fromString name)

#else

prop :: TestName -> Property -> TestTree
prop = testProperty

#endif