{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}

module Country.Unexposed.Util
  ( newZeroedByteArray
  , mapTextArray
  , charToWord8
  , word16ToChar
  , word16ToInt
  , timesTwo
  , timesThree
  , half
  ) where

import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Bits (unsafeShiftL,unsafeShiftR)
import Data.Char (chr,ord)
import Data.Primitive.ByteArray (MutableByteArray,newByteArray,fillByteArray)
import Data.Word (Word8,Word16)
import GHC.Exts (sizeofByteArray#)
import GHC.Int (Int(I#))

import qualified Data.Text.Array as TA

newZeroedByteArray :: PrimMonad m => Int -> m (MutableByteArray (PrimState m))
newZeroedByteArray len = do
  arr <- newByteArray len
  let arrStart = 0
  let zeroByte = 0
  fillByteArray arr arrStart len zeroByte
  pure arr
{-# INLINE newZeroedByteArray #-}

mapTextArray :: (Char -> Char) -> TA.Array -> TA.Array
mapTextArray f a@(TA.ByteArray inner) = TA.run $ do
  let len = I# (sizeofByteArray# inner)
  m <- TA.new len
  TA.copyI len m 0 a 0
  let go !ix = if ix < len
        then do
          TA.unsafeWrite m ix (charToWord8 (f (word8ToChar (TA.unsafeIndex a ix))))
          go (ix + 1)
        else return ()
  go 0
  return m
{-# INLINE mapTextArray #-}

charToWord8 :: Char -> Word8
charToWord8 = fromIntegral . ord
{-# INLINE charToWord8 #-}

word8ToChar :: Word8 -> Char
word8ToChar = chr . fromIntegral
{-# INLINE word8ToChar #-}

word16ToChar :: Word16 -> Char
word16ToChar = chr . fromIntegral
{-# INLINE word16ToChar #-}

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
