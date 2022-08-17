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
    getInt#,
    showInt#,

    -- ** Arithmetic #int-arithmetic#
    -- $section-int-arithmetic
    addInt#,
    subInt#,
    mulInt#,
    divInt#,
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

    -- * Int8# #int8#
    -- $section-int8
    Int8#,
    getInt8#,
    showInt8#,

    -- ** Arithmetic #int8-arithmetic#
    -- $section-int8-arithmetic
    addInt8#,
    subInt8#,
    mulInt8#,
    negateInt8#,
    absInt8#,
    signumInt8#,

    -- ** Comparison #int8-comparison#
    -- $section-int8-comparison
    eqInt8#,
    neInt8#,
    gtInt8#,
    geInt8#,
    ltInt8#,
    leInt8#,

    -- * Int16# #int16#
    -- $section-int16
    Int16#,
    getInt16#,
    showInt16#,

    -- ** Arithmetic #int16-arithmetic#
    -- $section-int16-arithmetic
    addInt16#,
    subInt16#,
    mulInt16#,
    negateInt16#,
    absInt16#,
    signumInt16#,

    -- ** Comparison #int16-comparison#
    -- $section-int16-comparison
    eqInt16#,
    neInt16#,
    gtInt16#,
    geInt16#,
    ltInt16#,
    leInt16#,

    -- * Int32# #int32#
    -- $section-int32
    Int32#,
    getInt32#,
    showInt32#,

    -- ** Arithmetic #int32-arithmetic#
    -- $section-int32-arithmetic
    addInt32#,
    subInt32#,
    mulInt32#,
    negateInt32#,
    absInt32#,
    signumInt32#,

    -- ** Comparison #int32-comparison#
    -- $section-int32-comparison
    eqInt32#,
    neInt32#,
    gtInt32#,
    geInt32#,
    ltInt32#,
    leInt32#,
  )
where

import Data.Bool.Prim (Bool#)

import qualified GHC.Classes as Prim (divInt#)
import GHC.Exts (unsafeCoerce#)
import GHC.Int (Int (I#), Int16 (I16#), Int32 (I32#), Int8 (I8#))
import GHC.Prim (Int#, Int16#, Int32#, Int8#)
import qualified GHC.Prim as Prim

--------------------------------------------------------------------------------

import Data.Int.Prim.Orphans ()

-- Int# ------------------------------------------------------------------------

-- $section-int #section-int#
--
-- TODO

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

-- Int# - Arithmetic -----------------------------------------------------------

-- $section-int-arithmetic
--
-- Primitive arithmetic operations on 'Int#'.

infixl 6 `addInt#`, `subInt#`
infixl 7 `mulInt#`, `divInt#`

-- | Addition of two 'Int#' values.
--
-- __NOTE:__ Integer overflow will wrap around 'maxBound' for 'Int'.
--
-- @since 1.0.0
addInt# :: Int# -> Int# -> Int#
addInt# a b = a Prim.+# b

-- | Subtraction of two 'Int#' values.
--
-- __NOTE:__ Integer underflow will wrap around 'minBound' for 'Int'.
--
-- @since 1.0.0
subInt# :: Int# -> Int# -> Int#
subInt# a b = a Prim.-# b

-- | Multiplication of two 'Int#' values.
--
-- __NOTE:__ Integer overflow will wrap around 'maxBound' for 'Int'.
--
-- @since 1.0.0
mulInt# :: Int# -> Int# -> Int#
mulInt# a b = a Prim.*# b

-- | Division of two 'Int#' values, rounded towards negative infinity.
--
-- __NOTE:__ This will fail with an unchecked exception if the divisor is zero.
--
-- @since 1.0.0
divInt# :: Int# -> Int# -> Int#
divInt# a b = Prim.divInt# a b

-- | Unary negation of an 'Int#' value.
--
-- @since 1.0.0
negateInt# :: Int# -> Int#
negateInt# = Prim.negateInt#

-- | Take the absolute value of an 'Int#'.
--
-- @since 1.0.0
absInt# :: Int# -> Int#
absInt# x =
  let tmp0 = Prim.uncheckedIShiftRL# x 31#
      tmp1 = Prim.andI# tmp0 1#
   in (Prim.xorI# x tmp0) Prim.+# tmp1

-- | Take the sign of an 'Int#' value resulting in either -1 (negative), 
-- 0 (zero) or 1 (positive).
--
-- @since 1.0.0
signumInt# :: Int# -> Int#
signumInt# x = (0# Prim.<# x) Prim.-# (x Prim.<# 0#)

-- Int# - Comparison -----------------------------------------------------------

-- $section-int-comparison
--
-- Primitive 'Int#' comparisons.

infix 4 `gtInt#`, `geInt#`, `eqInt#`, `neInt#`, `ltInt#`, `leInt#`

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

-- Int8# -----------------------------------------------------------------------

-- $section-int8 #section-int8#
--
-- TODO

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
addInt8# a b = Prim.plusInt8# a b

-- | Subtraction of two 'Int8#' values.
--
-- __NOTE:__ Integer underflow will wrap around @2^8 / 2@.
--
-- @since 1.0.0
subInt8# :: Int8# -> Int8# -> Int8#
subInt8# a b = Prim.subInt8# a b

-- | Multiplication of two 'Int8#' values.
--
-- __NOTE:__ Integer overflow will wrap around @2^8 / 2 - 1@.
--
-- @since 1.0.0
mulInt8# :: Int8# -> Int8# -> Int8#
mulInt8# a b = Prim.timesInt8# a b

-- | Unary negation of an 'Int8#' value.
--
-- @since 1.0.0
negateInt8# :: Int8# -> Int8#
negateInt8# = Prim.negateInt8#

-- | Take the absolute value of an 'Int8#'.
--
-- @since 1.0.0
absInt8# :: Int8# -> Int8#
absInt8# x = unsafeCoerce# (absInt# (unsafeCoerce# x))

-- | Take the sign of an 'Int8#' value resulting in either -1 (negative), 
-- 0 (zero) or 1 (positive).
--
-- @since 1.0.0
signumInt8# :: Int8# -> Int#
signumInt8# x = 
  let cmp0 = Prim.ltInt8# (unsafeCoerce# 0#) x
      cmp1 = Prim.ltInt8# x (unsafeCoerce# 0#)
   in cmp0 Prim.-# cmp1

-- Int8# - Comparison ----------------------------------------------------------

-- $section-int8-comparison
--
-- Primitive 'Int8#' comparisons.

infix 4 `gtInt8#`, `geInt8#`, `eqInt8#`, `neInt8#`, `ltInt8#`, `leInt8#`

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

-- Int16# ----------------------------------------------------------------------

-- $section-int16 #section-int16#
--
-- TODO

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
addInt16# a b = Prim.plusInt16# a b

-- | Subtraction of two 'Int16#' values.
--
-- __NOTE:__ Integer overflow will wrap around @2^16 / 2@.
--
-- @since 1.0.0
subInt16# :: Int16# -> Int16# -> Int16#
subInt16# a b = Prim.subInt16# a b

-- | Multiplication of two 'Int16#' values.
--
-- __NOTE:__ Integer overflow will wrap around @2^16 / 2 - 1@.
--
-- @since 1.0.0
mulInt16# :: Int16# -> Int16# -> Int16#
mulInt16# a b = Prim.timesInt16# a b

-- | Unary negation of an 'Int16#' value.
--
-- @since 1.0.0
negateInt16# :: Int16# -> Int16#
negateInt16# = Prim.negateInt16#

-- | Take the absolute value of an 'Int16#'.
--
-- @since 1.0.0
absInt16# :: Int16# -> Int16#
absInt16# x = unsafeCoerce# (absInt# (unsafeCoerce# x))

-- | Take the sign of an 'Int16#' value resulting in either -1 (negative), 
-- 0 (zero) or 1 (positive).
--
-- @since 1.0.0
signumInt16# :: Int16# -> Int#
signumInt16# x = 
  let cmp0 = Prim.ltInt16# (unsafeCoerce# 0#) x
      cmp1 = Prim.ltInt16# x (unsafeCoerce# 0#)
   in cmp0 Prim.-# cmp1

-- Int16# - Comparison ---------------------------------------------------------

-- $section-int16-comparison
--
-- Primitive 'Int16#' comparisons.

infix 4 `gtInt16#`, `geInt16#`, `eqInt16#`, `neInt16#`, `ltInt16#`, `leInt16#`

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

-- Int32# ----------------------------------------------------------------------

-- $section-int32 #section-int32#
--
-- TODO

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
addInt32# a b = Prim.plusInt32# a b

-- | Subtraction of two 'Int32#' values.
--
-- __NOTE:__ Integer overflow will wrap around @2^32 / 2@.
--
-- @since 1.0.0
subInt32# :: Int32# -> Int32# -> Int32#
subInt32# a b = Prim.subInt32# a b

-- | Multiplication of two 'Int32#' values.
--
-- __NOTE:__ Integer overflow will wrap around @2^32 / 2 - 1@.
--
-- @since 1.0.0
mulInt32# :: Int32# -> Int32# -> Int32#
mulInt32# a b = Prim.timesInt32# a b

-- | Unary negation of an 'Int32#' value.
--
-- @since 1.0.0
negateInt32# :: Int32# -> Int32#
negateInt32# = Prim.negateInt32#

-- | Take the absolute value of an 'Int32#'.
--
-- @since 1.0.0
absInt32# :: Int32# -> Int32#
absInt32# x = unsafeCoerce# (absInt# (unsafeCoerce# x))

-- | Take the sign of an 'Int32#' value resulting in either -1 (negative), 
-- 0 (zero) or 1 (positive).
--
-- @since 1.0.0
signumInt32# :: Int32# -> Int#
signumInt32# x = 
  let cmp0 = Prim.ltInt32# (unsafeCoerce# 0#) x
      cmp1 = Prim.ltInt32# x (unsafeCoerce# 0#)
   in cmp0 Prim.-# cmp1

-- Int32# - Comparison ---------------------------------------------------------

-- $section-int32-comparison
--
-- Primitive 'Int32#' comparisons.

infix 4 `gtInt32#`, `geInt32#`, `eqInt32#`, `neInt32#`, `ltInt32#`, `leInt32#`

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
