{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MagicHash #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Data.Int.Prim
-- Copyright   :  (c) Jacob Leach, 2022
-- License     :  ISC, see LICENSE
--
-- Maintainer  :  jacobleach@protonmail.com
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- This module exports a common set of primitives defined for each of the
-- unboxed integer types defined in "GHC.Prim".
--
-- * [@Int#@]("Data.Int.Unlifted#section-int")
-- * [@Int8#@]("Data.Int.Unlifted#section-int8")
-- * [@Int16#@]("Data.Int.Unlifted#section-int16")
-- * [@Int32#@]("Data.Int.Unlifted#section-int32")
-- * [@Int64#@]("Data.Int.Unlifted#section-int64")
--
-- @since 1.0.0
module Data.Int.Prim
  ( -- * Int# #int#
    -- $section-int
    Int#,
    Prim.Compat.fromInt#,
    Prim.Compat.toInt#,

    -- ** Arithmetic #int-arithmetic#
    -- $section-int-arithmetic
    addInt#,
    subInt#,
    mulInt#,
    negateInt#,
    absInt#,
    signumInt#,

    -- ** Comparison #int-comparison#
    -- $section-int-comparison
    eqInt#,
    neInt#,
    gtInt#,
    geInt#,
    ltInt#,
    leInt#,

    -- ** Show
    showInt#,

    -- * Int8# #int8#
    -- $section-int8
    Int8#,
    Prim.Compat.fromInt8#,
    Prim.Compat.toInt8#,

    -- ** Arithmetic #int8-arithmetic#
    -- $section-int8-arithmetic
    addInt8#,
    subInt8#,
    mulInt8#,
    negInt8#,
    signumInt8#,

    -- ** Comparison #int8-comparison#
    -- $section-int8-comparison
    eqInt8#,
    neInt8#,
    gtInt8#,
    geInt8#,
    ltInt8#,
    leInt8#,

    -- ** Conversion
    Prim.Compat.int8ToInt#,
    Prim.Compat.intToInt8#,

    -- ** Show
    showInt8#,

    -- * Int16# #int16#
    -- $section-int16
    Int16#,
    Prim.Compat.fromInt16#,
    Prim.Compat.toInt16#,

    -- ** Arithmetic #int16-arithmetic#
    -- $section-int16-arithmetic
    addInt16#,
    subInt16#,
    mulInt16#,
    negInt16#,
    signumInt16#,

    -- ** Comparison #int16-comparison#
    -- $section-int16-comparison
    eqInt16#,
    neInt16#,
    gtInt16#,
    geInt16#,
    ltInt16#,
    leInt16#,
    
    -- ** Conversion
    Prim.Compat.int16ToInt#,
    Prim.Compat.intToInt16#,

    -- ** Show
    showInt16#,

    -- * Int32# #int32#
    -- $section-int32
    Int32#,
    Prim.Compat.fromInt32#,
    Prim.Compat.toInt32#,

    -- ** Arithmetic #int32-arithmetic#
    -- $section-int32-arithmetic
    addInt32#,
    subInt32#,
    mulInt32#,
    negInt32#,
    signumInt32#,

    -- ** Comparison #int32-comparison#
    -- $section-int32-comparison
    eqInt32#,
    neInt32#,
    gtInt32#,
    geInt32#,
    ltInt32#,
    leInt32#,

    -- ** Conversion
    Prim.Compat.int32ToInt#,
    Prim.Compat.intToInt32#,

    -- ** Show
    showInt32#,
  )
where

import Data.Bool.Prim (Bool#)
import Data.Bool.Prim qualified as Bool
import Data.Int.Prim.Compat qualified as Prim.Compat

import GHC.Exts (Int#, Int16#, Int32#, Int8#)
import GHC.Exts qualified as GHC

import GHC.Int (Int (I#))

--------------------------------------------------------------------------------

import Data.Int.Prim.Orphans ()

-- Int# ------------------------------------------------------------------------

-- $section-int #section-int#
--
-- TODO

-- | Display an 'Int#' value as a 'String'.
--
-- >>> showInt# 12345#
-- "12345#"
--
-- @since 1.0.0
showInt# :: Int# -> String
showInt# x = shows (I# x) "#"

-- Int# - Arithmetic -----------------------------------------------------------

-- $section-int-arithmetic
--
-- Primitive arithmetic operations on 'Int#'.

infixl 6 `addInt#`, `subInt#`
infixl 7 `mulInt#`

-- | Addition of two 'Int#' values.
--
-- __NOTE:__ Integer overflow will wrap around 'maxBound' for 'Int'.
--
-- @since 1.0.0
addInt# :: Int# -> Int# -> Int#
addInt# = (GHC.+#)

-- | Subtraction of two 'Int#' values.
--
-- __NOTE:__ Integer underflow will wrap around 'minBound' for 'Int'.
--
-- @since 1.0.0
subInt# :: Int# -> Int# -> Int#
subInt# = (GHC.-#)

-- | Multiplication of two 'Int#' values.
--
-- __NOTE:__ Integer overflow will wrap around 'maxBound' for 'Int'.
--
-- @since 1.0.0
mulInt# :: Int# -> Int# -> Int#
mulInt# = (GHC.*#)

-- | Unary negation of an 'Int#' value.
--
-- @since 1.0.0
negateInt# :: Int# -> Int#
negateInt# = GHC.negateInt#

-- | Take the absolute value of an 'Int#'.
--
-- @since 1.0.0
absInt# :: Int# -> Int#
absInt# x =
  let tmp0 = GHC.uncheckedIShiftRL# x 31#
      tmp1 = GHC.andI# tmp0 1#
   in GHC.xorI# x tmp0 GHC.+# tmp1

-- | Take the sign of an 'Int#' value resulting in either -1 (negative),
-- 0 (zero) or 1 (positive).
--
-- @since 1.0.0
signumInt# :: Int# -> Int#
signumInt# x = (0# GHC.<# x) GHC.-# (x GHC.<# 0#)

-- Int# - Comparison -----------------------------------------------------------

-- $section-int-comparison
--
-- Primitive 'Int#' comparisons.

infix 4 `gtInt#`, `geInt#`, `eqInt#`, `neInt#`, `ltInt#`, `leInt#`

-- | "Greater than" comparison on two 'Int#' values.
--
-- @since 1.0.0
gtInt# :: Int# -> Int# -> Bool#
gtInt# a b = Bool.fromInt# (a GHC.># b)

-- | "Greater than or equal to" comparison on two 'Int#' values.
--
-- @since 1.0.0
geInt# :: Int# -> Int# -> Bool#
geInt# a b = Bool.fromInt# (a GHC.>=# b)

-- | "Equal to" comparison on two 'Int#' values.
--
-- @since 1.0.0
eqInt# :: Int# -> Int# -> Bool#
eqInt# a b = Bool.fromInt# (a GHC.==# b)

-- | "Not equal to" comparison on two 'Int#' values.
--
-- @since 1.0.0
neInt# :: Int# -> Int# -> Bool#
neInt# a b = Bool.fromInt# (a GHC./=# b)

-- | "Less than" comparison on two 'Int#' values.
--
-- @since 1.0.0
ltInt# :: Int# -> Int# -> Bool#
ltInt# a b = Bool.fromInt# (a GHC.<# b)

-- | "Less than or equal to" comparison on two 'Int#' values.
--
-- @since 1.0.0
leInt# :: Int# -> Int# -> Bool#
leInt# a b = Bool.fromInt# (a GHC.<=# b)

-- Int8# -----------------------------------------------------------------------

-- $section-int8 #section-int8#
--
-- TODO

-- | Display an 'Int8#' value as a 'String'.
--
-- >>> showInt8# (intToInt8# 12345#)
-- "12345#"
--
-- @since 1.0.0
showInt8# :: Int8# -> String
showInt8# x = shows (I# (Prim.Compat.int8ToInt# x)) "#"

-- Int8# - Arithmetic ----------------------------------------------------------

-- $section-int8-arithmetic
--
-- Primitive arithmetic operations on 'Int8#'.

infixl 6 `addInt8#`, `subInt8#`
infixl 7 `mulInt8#`

-- | Addition of two 'Int8#' values.
--
-- __NOTE:__ Integer overflow will wrap around @2^8 / 2 - 1@.
--
-- @since 1.0.0
addInt8# :: Int8# -> Int8# -> Int8#
addInt8# = GHC.plusInt8#

-- | Subtraction of two 'Int8#' values.
--
-- __NOTE:__ Integer underflow will wrap around @2^8 / 2@.
--
-- @since 1.0.0
subInt8# :: Int8# -> Int8# -> Int8#
subInt8# = GHC.subInt8#

-- | Multiplication of two 'Int8#' values.
--
-- __NOTE:__ Integer overflow will wrap around @2^8 / 2 - 1@.
--
-- @since 1.0.0
mulInt8# :: Int8# -> Int8# -> Int8#
mulInt8# = GHC.timesInt8#

-- | Unary negation of an 'Int8#' value.
--
-- @since 1.0.0
negInt8# :: Int8# -> Int8#
negInt8# = GHC.negateInt8#

-- | Take the sign of an 'Int8#' value resulting in either -1 (negative),
-- 0 (zero) or 1 (positive).
--
-- @since 1.0.0
signumInt8# :: Int8# -> Int#
signumInt8# x =
  let cmp0 = GHC.ltInt8# (Prim.Compat.intToInt8# 0#) x
      cmp1 = GHC.ltInt8# x (Prim.Compat.intToInt8# 0#)
   in cmp0 GHC.-# cmp1

-- Int8# - Comparison ----------------------------------------------------------

-- $section-int8-comparison
--
-- Primitive 'Int8#' comparisons.

infix 4 `gtInt8#`, `geInt8#`, `eqInt8#`, `neInt8#`, `ltInt8#`, `leInt8#`

-- | "Equal to" comparison on two 'Int8#' values.
--
-- @since 1.0.0
eqInt8# :: Int8# -> Int8# -> Bool#
eqInt8# a b = Bool.fromInt# (GHC.eqInt8# a b)

-- | "Not equal to" comparison on two 'Int8#' values.
--
-- @since 1.0.0
neInt8# :: Int8# -> Int8# -> Bool#
neInt8# a b = Bool.fromInt# (GHC.neInt8# a b)

-- | "Greater than" comparison on two 'Int8#' values.
--
-- @since 1.0.0
gtInt8# :: Int8# -> Int8# -> Bool#
gtInt8# a b = Bool.fromInt# (GHC.gtInt8# a b)

-- | "Greater than or equal to" comparison on two 'Int8#' values.
--
-- @since 1.0.0
geInt8# :: Int8# -> Int8# -> Bool#
geInt8# a b = Bool.fromInt# (GHC.geInt8# a b)

-- | "Less than" comparison on two 'Int8#' values.
--
-- @since 1.0.0
ltInt8# :: Int8# -> Int8# -> Bool#
ltInt8# a b = Bool.fromInt# (GHC.ltInt8# a b)

-- | "Less than or equal to" comparison on two 'Int8#' values.
--
-- @since 1.0.0
leInt8# :: Int8# -> Int8# -> Bool#
leInt8# a b = Bool.fromInt# (GHC.leInt8# a b)

-- Int16# ----------------------------------------------------------------------

-- $section-int16 #section-int16#
--
-- TODO

-- | Display an 'Int16#' value as a 'String'.
--
-- >>> showInt16# (intToInt16# 12345#)
-- "12345#"
--
-- @since 1.0.0
showInt16# :: Int16# -> String
showInt16# x = shows (I# (Prim.Compat.int16ToInt# x)) "#"

-- Int16# - Arithmetic ---------------------------------------------------------

-- $section-int16-arithmetic
--
-- Primitive arithmetic operations on 'Int8#'.

infixl 6 `addInt16#`, `subInt16#`
infixl 7 `mulInt16#`

-- | Addition of two 'Int16#' values.
--
-- __NOTE:__ Integer overflow will wrap around @2^16 / 2 - 1@.
--
-- @since 1.0.0
addInt16# :: Int16# -> Int16# -> Int16#
addInt16# = GHC.plusInt16#

-- | Subtraction of two 'Int16#' values.
--
-- __NOTE:__ Integer overflow will wrap around @2^16 / 2@.
--
-- @since 1.0.0
subInt16# :: Int16# -> Int16# -> Int16#
subInt16# = GHC.subInt16#

-- | Multiplication of two 'Int16#' values.
--
-- __NOTE:__ Integer overflow will wrap around @2^16 / 2 - 1@.
--
-- @since 1.0.0
mulInt16# :: Int16# -> Int16# -> Int16#
mulInt16# = GHC.timesInt16#

-- | Unary negation of an 'Int16#' value.
--
-- @since 1.0.0
negInt16# :: Int16# -> Int16#
negInt16# = GHC.negateInt16#

-- | Take the sign of an 'Int16#' value resulting in either -1 (negative),
-- 0 (zero) or 1 (positive).
--
-- @since 1.0.0
signumInt16# :: Int16# -> Int#
signumInt16# x =
  let cmp0 = GHC.ltInt16# (Prim.Compat.intToInt16# 0#) x
      cmp1 = GHC.ltInt16# x (Prim.Compat.intToInt16# 0#)
   in cmp0 GHC.-# cmp1

-- Int16# - Comparison ---------------------------------------------------------

-- $section-int16-comparison
--
-- Primitive 'Int16#' comparisons.

infix 4 `gtInt16#`, `geInt16#`, `eqInt16#`, `neInt16#`, `ltInt16#`, `leInt16#`

-- | "Equal to" comparison on two 'Int16#' values.
--
-- @since 1.0.0
eqInt16# :: Int16# -> Int16# -> Bool#
eqInt16# a b = Bool.fromInt# (GHC.eqInt16# a b)

-- | "Not equal to" comparison on two 'Int16#' values.
--
-- @since 1.0.0
neInt16# :: Int16# -> Int16# -> Bool#
neInt16# a b = Bool.fromInt# (GHC.neInt16# a b)

-- | "Greater than" comparison on two 'Int16#' values.
--
-- @since 1.0.0
gtInt16# :: Int16# -> Int16# -> Bool#
gtInt16# a b = Bool.fromInt# (GHC.gtInt16# a b)

-- | "Greater than or equal to" comparison on two 'Int16#' values.
--
-- @since 1.0.0
geInt16# :: Int16# -> Int16# -> Bool#
geInt16# a b = Bool.fromInt# (GHC.geInt16# a b)

-- | "Less than" comparison on two 'Int16#' values.
--
-- @since 1.0.0
ltInt16# :: Int16# -> Int16# -> Bool#
ltInt16# a b = Bool.fromInt# (GHC.ltInt16# a b)

-- | "Less than or equal to" comparison on two 'Int16#' values.
--
-- @since 1.0.0
leInt16# :: Int16# -> Int16# -> Bool#
leInt16# a b = Bool.fromInt# (GHC.leInt16# a b)

-- Int32# ----------------------------------------------------------------------

-- $section-int32 #section-int32#
--
-- TODO

-- | Display an 'Int32#' value as a 'String'.
--
-- >>> showInt32# (intToInt32# 12345#)
-- "12345#"
--
-- @since 1.0.0
showInt32# :: Int32# -> String
showInt32# x = shows (I# (Prim.Compat.int32ToInt# x)) "#"

-- Int32# - Arithmetic ---------------------------------------------------------

-- $section-int32-arithmetic
--
-- Primitive arithmetic operations on 'Int8#'.

infixl 6 `addInt32#`, `subInt32#`
infixl 7 `mulInt32#`

-- | Addition of two 'Int32#' values.
--
-- __NOTE:__ Integer overflow will wrap around @2^32 / 2 - 1@.
--
-- @since 1.0.0
addInt32# :: Int32# -> Int32# -> Int32#
addInt32# = GHC.plusInt32#

-- | Subtraction of two 'Int32#' values.
--
-- __NOTE:__ Integer overflow will wrap around @2^32 / 2@.
--
-- @since 1.0.0
subInt32# :: Int32# -> Int32# -> Int32#
subInt32# = GHC.subInt32#

-- | Multiplication of two 'Int32#' values.
--
-- __NOTE:__ Integer overflow will wrap around @2^32 / 2 - 1@.
--
-- @since 1.0.0
mulInt32# :: Int32# -> Int32# -> Int32#
mulInt32# = GHC.timesInt32#

-- | Unary negation of an 'Int32#' value.
--
-- @since 1.0.0
negInt32# :: Int32# -> Int32#
negInt32# = GHC.negateInt32#

-- | Take the sign of an 'Int32#' value resulting in either -1 (negative),
-- 0 (zero) or 1 (positive).
--
-- @since 1.0.0
signumInt32# :: Int32# -> Int#
signumInt32# x =
  let cmp0 = GHC.ltInt32# (Prim.Compat.intToInt32# 0#) x
      cmp1 = GHC.ltInt32# x (Prim.Compat.intToInt32# 0#)
   in cmp0 GHC.-# cmp1

-- Int32# - Comparison ---------------------------------------------------------

-- $section-int32-comparison
--
-- Primitive 'Int32#' comparisons.

infix 4 `gtInt32#`, `geInt32#`, `eqInt32#`, `neInt32#`, `ltInt32#`, `leInt32#`

-- | "Equal to" comparison on two 'Int32#' values.
--
-- @since 1.0.0
eqInt32# :: Int32# -> Int32# -> Bool#
eqInt32# a b = Bool.fromInt# (Prim.Compat.eqInt32# a b)

-- | "Not equal to" comparison on two 'Int32#' values.
--
-- @since 1.0.0
neInt32# :: Int32# -> Int32# -> Bool#
neInt32# a b = Bool.fromInt# (Prim.Compat.neInt32# a b)

-- | "Greater than" comparison on two 'Int32#' values.
--
-- @since 1.0.0
gtInt32# :: Int32# -> Int32# -> Bool#
gtInt32# a b = Bool.fromInt# (Prim.Compat.gtInt32# a b)

-- | "Greater than or equal to" comparison on two 'Int32#' values.
--
-- @since 1.0.0
geInt32# :: Int32# -> Int32# -> Bool#
geInt32# a b = Bool.fromInt# (Prim.Compat.geInt32# a b)

-- | "Less than" comparison on two 'Int32#' values.
--
-- @since 1.0.0
ltInt32# :: Int32# -> Int32# -> Bool#
ltInt32# a b = Bool.fromInt# (Prim.Compat.ltInt32# a b)

-- | "Less than or equal to" comparison on two 'Int32#' values.
--
-- @since 1.0.0
leInt32# :: Int32# -> Int32# -> Bool#
leInt32# a b = Bool.fromInt# (Prim.Compat.leInt32# a b)
