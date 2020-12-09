{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Continent
  ( Continent
  , pattern Africa
  , pattern Asia
  , pattern Antarctica
  , pattern Europe
  , pattern NorthAmerica
  , pattern Oceania
  , pattern SouthAmerica
  -- * Name
  , encodeEnglish
  -- * Two-letter Codes
  , alphaUpper
  , alphaLower
  ) where

import Continent.Unsafe

import Control.Monad (forM_)
import Control.Monad.ST (runST)
import Country.Unexposed.Util (charToWord16,mapTextArray,timesTwo)
import Data.Char (toLower)
import Data.Text (Text)

import qualified Data.Primitive as Prim
import qualified Data.Text.Array as TA
import qualified Data.Text.Internal as TI


numberOfContinents :: Int
numberOfContinents = length continentNameDb
{-# NOINLINE numberOfContinents #-}


alphaUpper :: Continent -> Text
alphaUpper (Continent n) = TI.text allAlphaUpper (timesTwo (fromIntegral n)) 2

allAlphaUpper :: TA.Array
allAlphaUpper = TA.run $ do
  m <- TA.new (timesTwo numberOfContinents)
  forM_ continentNameDb $ \(n,_,(a1,a2)) -> do
    let ix = timesTwo (fromIntegral n)
    TA.unsafeWrite m ix (charToWord16 a1)
    TA.unsafeWrite m (ix + 1) (charToWord16 a2)
  return m
{-# NOINLINE allAlphaUpper #-}

alphaLower :: Continent -> Text
alphaLower (Continent n) = TI.text allAlphaLower (timesTwo (fromIntegral n)) 2

allAlphaLower :: TA.Array
allAlphaLower = mapTextArray toLower allAlphaUpper
{-# NOINLINE allAlphaLower #-}

encodeEnglish :: Continent -> Text
encodeEnglish (Continent n) = Prim.indexArray englishContinentNamesText (fromIntegral n)

englishContinentNamesText :: Prim.Array Text
englishContinentNamesText = runST $ do
  m <- Prim.newArray numberOfContinents unnamed
  mapM_ (\(ix,name,_) -> Prim.writeArray m (fromIntegral ix) name) continentNameDb
  Prim.unsafeFreezeArray m
{-# NOINLINE englishContinentNamesText #-}

unnamed :: Text
unnamed = "Invalid Continent"
{-# NOINLINE unnamed #-}
