{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternSynonyms #-}

-- |
-- Module      :  Data.Int.Unlifted
-- Copyright   :  (c) Jacob Leach, 2022
-- License     :  see LICENSE
--
-- Maintainer  :  jacobleach@protonmail.com
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- This module exports a common set of functions defined for each of the
-- unlifted integer types 'Int#', 'Int8#', 'Int16#', 'Int32#' and 'Int64#'.
--
-- @since 1.0.0
module Data.Int.Unlifted
  ( -- * Int#
    Int#,
    getInt#,
    showInt#,

    -- ** Comparison
    eqInt#,
    neInt#,
    gtInt#,
    geInt#,
    ltInt#,
    leInt#,

    -- * Int8#
    Int8#,
    getInt8#,
    showInt8#,

    -- ** Comparison
    eqInt8#,
    neInt8#,
    gtInt8#,
    geInt8#,
    ltInt8#,
    leInt8#,

    -- * Int16#
    Int16#,
    getInt16#,
    showInt16#,

    -- ** Comparison
    eqInt16#,
    neInt16#,
    gtInt16#,
    geInt16#,
    ltInt16#,
    leInt16#,

    -- * Int32#
    Int32#,
    getInt32#,
    showInt32#,

    -- ** Comparison
    eqInt32#,
    neInt32#,
    gtInt32#,
    geInt32#,
    ltInt32#,
    leInt32#,

    -- * Int64#
    Int64#,
  )
where

---------------------------------------------------------------------------------

import Data.Bool.Unlifted (Bool#)

import GHC.Exts (unsafeCoerce#)
import GHC.Int (Int (I#), Int16 (I16#), Int32 (I32#), Int8 (I8#))
import GHC.Prim (Int#, Int16#, Int32#, Int64#, Int8#)
import qualified GHC.Prim as Prim

-- Int# -------------------------------------------------------------------------

-- | Unbox an 'Int' value.
--
-- @since 1.0.0
getInt# :: Int -> Int#
getInt# (I# x) = x

-- | Display an 'Int#' value as a 'String'.
--
-- >>> showInt# 12345#
-- "12345#"
--
-- @since 1.0.0
showInt# :: Int# -> String
showInt# x = shows (I# x) "#"

-- Int# - Comparison -----------------------------------------------------------

infix 4 `eqInt#`, `neInt#`

-- | "Equal to" comparison on two 'Int#' values.
--
-- @since 1.0.0
eqInt# :: Int# -> Int# -> Bool#
eqInt# a b = unsafeCoerce# (a Prim.==# b)

-- | "Not equal to" comparison on two 'Int#' values.
--
-- @since 1.0.0
neInt# :: Int# -> Int# -> Bool#
neInt# a b = unsafeCoerce# (a Prim./=# b)

infix 4 `gtInt#`, `geInt#`, `ltInt#`, `leInt#`

-- | "Greater than" comparison on two 'Int#' values.
--
-- @since 1.0.0
gtInt# :: Int# -> Int# -> Bool#
gtInt# a b = unsafeCoerce# (a Prim.># b)

-- | "Greater than or equal to" comparison on two 'Int#' values.
--
-- @since 1.0.0
geInt# :: Int# -> Int# -> Bool#
geInt# a b = unsafeCoerce# (a Prim.>=# b)

-- | "Less than" comparison on two 'Int#' values.
--
-- @since 1.0.0
ltInt# :: Int# -> Int# -> Bool#
ltInt# a b = unsafeCoerce# (a Prim.<# b)

-- | "Less than or equal to" comparison on two 'Int#' values.
--
-- @since 1.0.0
leInt# :: Int# -> Int# -> Bool#
leInt# a b = unsafeCoerce# (a Prim.<=# b)

-- Int8# ------------------------------------------------------------------------

-- | Unbox an 'Int8' value.
--
-- @since 1.0.0
getInt8# :: Int8 -> Int8#
getInt8# (I8# x) = x

-- | Display an 'Int8#' value as a 'String'.
--
-- >>> showInt8# (intToInt8# 12345#)
-- "12345#"
--
-- @since 1.0.0
showInt8# :: Int8# -> String
showInt8# x = shows (I8# x) "#"

-- Int8# - Comparison ----------------------------------------------------------

infix 4 `eqInt8#`, `neInt8#`

-- | "Equal to" comparison on two 'Int8#' values.
--
-- @since 1.0.0
eqInt8# :: Int8# -> Int8# -> Bool#
eqInt8# a b = unsafeCoerce# (Prim.eqInt8# a b)

-- | "Not equal to" comparison on two 'Int8#' values.
--
-- @since 1.0.0
neInt8# :: Int8# -> Int8# -> Bool#
neInt8# a b = unsafeCoerce# (Prim.neInt8# a b)

infix 4 `gtInt8#`, `geInt8#`, `ltInt8#`, `leInt8#`

-- | "Greater than" comparison on two 'Int8#' values.
--
-- @since 1.0.0
gtInt8# :: Int8# -> Int8# -> Bool#
gtInt8# a b = unsafeCoerce# (Prim.gtInt8# a b)

-- | "Greater than or equal to" comparison on two 'Int8#' values.
--
-- @since 1.0.0
geInt8# :: Int8# -> Int8# -> Bool#
geInt8# a b = unsafeCoerce# (Prim.geInt8# a b)

-- | "Less than" comparison on two 'Int8#' values.
--
-- @since 1.0.0
ltInt8# :: Int8# -> Int8# -> Bool#
ltInt8# a b = unsafeCoerce# (Prim.ltInt8# a b)

-- | "Less than or equal to" comparison on two 'Int8#' values.
--
-- @since 1.0.0
leInt8# :: Int8# -> Int8# -> Bool#
leInt8# a b = unsafeCoerce# (Prim.leInt8# a b)

-- Int16# -----------------------------------------------------------------------

-- | Unbox an 'Int16' value.
--
-- @since 1.0.0
getInt16# :: Int16 -> Int16#
getInt16# (I16# x) = x

-- | Display an 'Int16#' value as a 'String'.
--
-- >>> showInt16# (intToInt16# 12345#)
-- "12345#"
--
-- @since 1.0.0
showInt16# :: Int16# -> String
showInt16# x = shows (I16# x) "#"

-- Int16# - Comparison ---------------------------------------------------------

infix 4 `eqInt16#`, `neInt16#`

-- | "Equal to" comparison on two 'Int16#' values.
--
-- @since 1.0.0
eqInt16# :: Int16# -> Int16# -> Bool#
eqInt16# a b = unsafeCoerce# (Prim.eqInt16# a b)

-- | "Not equal to" comparison on two 'Int16#' values.
--
-- @since 1.0.0
neInt16# :: Int16# -> Int16# -> Bool#
neInt16# a b = unsafeCoerce# (Prim.neInt16# a b)

infix 4 `gtInt16#`, `geInt16#`, `ltInt16#`, `leInt16#`

-- | "Greater than" comparison on two 'Int16#' values.
--
-- @since 1.0.0
gtInt16# :: Int16# -> Int16# -> Bool#
gtInt16# a b = unsafeCoerce# (Prim.gtInt16# a b)

-- | "Greater than or equal to" comparison on two 'Int16#' values.
--
-- @since 1.0.0
geInt16# :: Int16# -> Int16# -> Bool#
geInt16# a b = unsafeCoerce# (Prim.geInt16# a b)

-- | "Less than" comparison on two 'Int16#' values.
--
-- @since 1.0.0
ltInt16# :: Int16# -> Int16# -> Bool#
ltInt16# a b = unsafeCoerce# (Prim.ltInt16# a b)

-- | "Less than or equal to" comparison on two 'Int16#' values.
--
-- @since 1.0.0
leInt16# :: Int16# -> Int16# -> Bool#
leInt16# a b = unsafeCoerce# (Prim.leInt16# a b)

-- Int32# -----------------------------------------------------------------------

-- | Unbox an 'Int32' value.
--
-- @since 1.0.0
getInt32# :: Int32 -> Int32#
getInt32# (I32# x) = x

-- | Display an 'Int32#' value as a 'String'.
--
-- >>> showInt32# (intToInt32# 12345#)
-- "12345#"
--
-- @since 1.0.0
showInt32# :: Int32# -> String
showInt32# x = shows (I32# x) "#"

-- Int32# - Comparison ---------------------------------------------------------

infix 4 `eqInt32#`, `neInt32#`

-- | "Equal to" comparison on two 'Int32#' values.
--
-- @since 1.0.0
eqInt32# :: Int32# -> Int32# -> Bool#
eqInt32# a b = unsafeCoerce# (Prim.eqInt32# a b)

-- | "Not equal to" comparison on two 'Int32#' values.
--
-- @since 1.0.0
neInt32# :: Int32# -> Int32# -> Bool#
neInt32# a b = unsafeCoerce# (Prim.neInt32# a b)

infix 4 `gtInt32#`, `geInt32#`, `ltInt32#`, `leInt32#`

-- | "Greater than" comparison on two 'Int32#' values.
--
-- @since 1.0.0
gtInt32# :: Int32# -> Int32# -> Bool#
gtInt32# a b = unsafeCoerce# (Prim.gtInt32# a b)

-- | "Greater than or equal to" comparison on two 'Int32#' values.
--
-- @since 1.0.0
geInt32# :: Int32# -> Int32# -> Bool#
geInt32# a b = unsafeCoerce# (Prim.geInt32# a b)

-- | "Less than" comparison on two 'Int32#' values.
--
-- @since 1.0.0
ltInt32# :: Int32# -> Int32# -> Bool#
ltInt32# a b = unsafeCoerce# (Prim.ltInt32# a b)

-- | "Less than or equal to" comparison on two 'Int32#' values.
--
-- @since 1.0.0
leInt32# :: Int32# -> Int32# -> Bool#
leInt32# a b = unsafeCoerce# (Prim.leInt32# a b)
