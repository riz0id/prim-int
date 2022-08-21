{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Module      :  Data.Int.Prim.Orphans
-- Copyright   :  (c) Jacob Leach, 2022
-- License     :  ISC, see LICENSE
--
-- Maintainer  :  jacobleach@protonmail.com
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- This file contains the orphan instances re-exported by "Data.Int.Prim".
--
-- @since 1.0.0
module Data.Int.Prim.Orphans () where

import Data.Int.Prim.Compat
  ( int16ToInt#,
    int32ToInt#,
    int8ToInt#,
    intToInt16#,
    intToInt32#,
    intToInt8#,
    toInt,
  )

import GHC.Exts (Int (I#), Int16#, Int32#, Int8#)

import Language.Haskell.TH.Syntax
  ( Exp (AppE, LitE, VarE),
    Lift (lift, liftTyped),
    Lit (IntPrimL),
    unsafeCodeCoerce,
  )

-- Int8# - Orphan Instances ----------------------------------------------------

-- | /Orphan defined in "Data.Int" since @prim-int-1.0.0@./
--
-- @since 1.0.0
instance Lift Int8# where
  lift x =
    let conE = VarE 'intToInt8#
        litE = LitE (IntPrimL (fromIntegral (I# (int8ToInt# x))))
     in pure (conE `AppE` VarE 'toInt `AppE` litE)

  liftTyped x = unsafeCodeCoerce (lift x)

-- Int16# - Orphan Instances ---------------------------------------------------

-- | /Orphan defined in "Data.Int" since @prim-int-1.0.0@./
--
-- @since 1.0.0
instance Lift Int16# where
  lift x =
    let conE = VarE 'intToInt16#
        litE = LitE (IntPrimL (fromIntegral (I# (int16ToInt# x))))
     in pure (conE `AppE` VarE 'toInt `AppE` litE)

  liftTyped x = unsafeCodeCoerce (lift x)

-- Int32# - Orphan Instances ---------------------------------------------------

-- | /Orphan defined in "Data.Int" since @prim-int-1.0.0@./
--
-- @since 1.0.0
instance Lift Int32# where
  lift x =
    let conE = VarE 'intToInt32#
        litE = LitE (IntPrimL (fromIntegral (I# (int32ToInt# x))))
     in pure (conE `AppE` VarE 'toInt `AppE` litE)

  liftTyped x = unsafeCodeCoerce (lift x)