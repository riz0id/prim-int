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

import GHC.Exts (unsafeCoerce#)
import GHC.Int (Int16 (I16#), Int32 (I32#), Int8 (I8#))
import GHC.Prim (Int16#, Int32#, Int8#)

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
  lift x = pure (AppE (VarE 'unsafeCoerce#) (LitE (IntPrimL (fromIntegral (I8# x)))))
  {-# INLINE lift #-}

  liftTyped x = unsafeCodeCoerce (lift x)
  {-# INLINE liftTyped #-}

-- Int16# - Orphan Instances ---------------------------------------------------

-- | /Orphan defined in "Data.Int" since @prim-int-1.0.0@./
--
-- @since 1.0.0
instance Lift Int16# where
  lift x = pure (AppE (VarE 'unsafeCoerce#) (LitE (IntPrimL (fromIntegral (I16# x)))))
  {-# INLINE lift #-}

  liftTyped x = unsafeCodeCoerce (lift x)
  {-# INLINE liftTyped #-}

-- Int32# - Orphan Instances ---------------------------------------------------

-- | /Orphan defined in "Data.Int" since @prim-int-1.0.0@./
--
-- @since 1.0.0
instance Lift Int32# where
  lift x = pure (AppE (VarE 'unsafeCoerce#) (LitE (IntPrimL (fromIntegral (I32# x)))))
  {-# INLINE lift #-}

  liftTyped x = unsafeCodeCoerce (lift x)
  {-# INLINE liftTyped #-}