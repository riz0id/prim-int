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

#if (MIN_VERSION_ghc_prim(0,8,0))

import GHC.Int (Int16 (I16#), Int32 (I32#), Int8 (I8#))
import GHC.Prim (Int16#, Int32#, Int8#)

#else

import GHC.Int (Int (I#))
import GHC.Prim (Int16#, Int8#)
import qualified GHC.Prim as Prim 

#endif 

import Language.Haskell.TH.Syntax
  ( Exp (AppE, LitE, VarE),
    Lift (lift, liftTyped),
    Lit (IntPrimL),
    unsafeCodeCoerce,
  )

--------------------------------------------------------------------------------

-- Int8# - Orphan Instances ----------------------------------------------------

-- | /Orphan defined in "Data.Int" since @prim-int-1.0.0@./
--
-- @since 1.0.0
instance Lift Int8# where

#if (MIN_VERSION_ghc_prim(0,8,0))

  lift x = pure (AppE (VarE 'unsafeCoerce#) (LitE (IntPrimL (fromIntegral (I8# x)))))

#else

  lift x = pure (AppE (VarE 'unsafeCoerce#) (LitE (IntPrimL (fromIntegral (I# (Prim.extendInt8# x))))))

#endif

  liftTyped x = unsafeCodeCoerce (lift x)

-- Int16# - Orphan Instances ---------------------------------------------------

-- | /Orphan defined in "Data.Int" since @prim-int-1.0.0@./
--
-- @since 1.0.0
instance Lift Int16# where

#if (MIN_VERSION_ghc_prim(0,8,0))

  lift x = pure (AppE (VarE 'unsafeCoerce#) (LitE (IntPrimL (fromIntegral (I16# x)))))

#else

  lift x = pure (AppE (VarE 'unsafeCoerce#) (LitE (IntPrimL (fromIntegral (I# (Prim.extendInt16# x))))))

#endif

  liftTyped x = unsafeCodeCoerce (lift x)

-- Int32# - Orphan Instances ---------------------------------------------------

#if (MIN_VERSION_ghc_prim(0,8,0))

-- | /Orphan defined in "Data.Int" since @prim-int-1.0.0@./
--
-- @since 1.0.0
instance Lift Int32# where
  lift x = pure (AppE (VarE 'unsafeCoerce#) (LitE (IntPrimL (fromIntegral (I32# x)))))

  liftTyped x = unsafeCodeCoerce (lift x)

#endif