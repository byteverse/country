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

import qualified Data.Text.Array as TA


mapTextArray :: (Char -> Char) -> TA.Array -> TA.Array
#if MIN_VERSION_base(4,17,0)
mapTextArray f a@(TA.ByteArray inner) = TA.run $ do
#else
mapTextArray f a@(TA.Array inner) = TA.run $ do
#endif
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
