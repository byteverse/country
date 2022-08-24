{-# language RankNTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}

module Country.Unexposed.Util
  ( mapTextArray
  , textWordToChar
  , charToTextWord
  , charToWord16
  , word16ToChar
  , charToWord8
  , word8ToChar
  , word16ToInt
  , timesTwo
  , timesThree
  , half
  ) where

import Data.Bits (unsafeShiftL,unsafeShiftR)
import Data.Char (chr,ord)
import Data.Word (Word16, Word8)
import GHC.Exts (sizeofByteArray#)
import GHC.Int (Int(I#))
import Control.Monad.ST
import Data.Text.Unsafe
import Data.Text.Internal
import qualified Data.Text.Internal.Unsafe.Char as Unsafe.Char

import qualified Data.Text.Array as TA


mapTextArray :: (Char -> Char) -> TA.Array -> TA.Array
#if MIN_VERSION_text(2,0,0)
mapTextArray f src@(TA.ByteArray arr) = runST $ do
    -- this implementation is lifted from the text internals
      marr <- TA.new (l + 4)
      outer marr (l + 4) o 0
      where
        o = 0
        l = I# (sizeofByteArray# arr)
        outer :: forall s. TA.MArray s -> Int -> Int -> Int -> ST s TA.Array
        outer !dst !dstLen = inner
          where
            inner !srcOff !dstOff
              | srcOff >= l + o = do
                TA.shrinkM dst dstOff
                arr <- TA.unsafeFreeze dst
                return arr
              | dstOff + 4 > dstLen = do
                let !dstLen' = dstLen + (l + o) - srcOff + 4
                dst' <- TA.resizeM dst dstLen'
                outer dst' dstLen' srcOff dstOff
              | otherwise = do
                let !(Iter c d) = iterArray src srcOff
                d' <- Unsafe.Char.unsafeWrite dst dstOff (safe (f c))
                inner (srcOff + d) (dstOff + d')
#else
mapTextArray f a@(TA.Array inner) = TA.run $ do
  let len = half (I# (sizeofByteArray# inner))
  m <- TA.new len
  TA.copyI len m 0 a 0
  let go !ix = if ix < len
        then do
          TA.unsafeWrite m ix (charToTextWord (f (textWordToChar (TA.unsafeIndex a ix))))
          go (ix + 1)
        else return ()
  go 0
  return m
#endif
{-# INLINE mapTextArray #-}

#if MIN_VERSION_text(2,0,0)
textWordToChar :: Word8 -> Char
textWordToChar = word8ToChar

charToTextWord :: Char -> Word8
charToTextWord = charToWord8
#else
textWordToChar :: Word16 -> Char
textWordToChar = word16ToChar

charToTextWord :: Char -> Word16
charToTextWord = charToWord16
#endif

word8ToChar :: Word8 -> Char
word8ToChar = chr . fromIntegral
{-# INLINE word8ToChar #-}

charToWord8 :: Char -> Word8
charToWord8 = fromIntegral . ord
{-# INLINE charToWord8 #-}

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
