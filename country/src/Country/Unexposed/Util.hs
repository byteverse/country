{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}

module Country.Unexposed.Util
  ( mapTextArray
  , charToWord16
  , word16ToChar
  , word16ToInt
  , timesTwo
  , timesThree
  , half
  ) where

import Data.Bits (unsafeShiftL,unsafeShiftR)
import Data.Char (chr,ord)
import Data.Word (Word16)
import GHC.Exts (sizeofByteArray#)
import GHC.Int (Int(I#))

import qualified Data.Text.Array as TA


mapTextArray :: (Char -> Char) -> TA.Array -> TA.Array
mapTextArray f a@(TA.Array inner) = TA.run $ do
  let len = half (I# (sizeofByteArray# inner))
  m <- TA.new len
  TA.copyI m 0 a 0 len
  let go !ix = if ix < len
        then do
          TA.unsafeWrite m ix (charToWord16 (f (word16ToChar (TA.unsafeIndex a ix))))
          go (ix + 1)
        else return ()
  go 0
  return m
{-# INLINE mapTextArray #-}

word16ToChar :: Word16 -> Char
word16ToChar = chr . fromIntegral
{-# INLINE word16ToChar #-}

charToWord16 :: Char -> Word16
charToWord16 = fromIntegral . ord
{-# INLINE charToWord16 #-}

word16ToInt :: Word16 -> Int
word16ToInt = fromIntegral
{-# INLINE word16ToInt #-}

timesTwo :: Int -> Int
timesTwo x = unsafeShiftL x 1
{-# INLINE timesTwo #-}

timesThree :: Int -> Int
timesThree x = x + timesTwo x
{-# INLINE timesThree #-}

half :: Int -> Int
half x = unsafeShiftR x 1
{-# INLINE half #-}
