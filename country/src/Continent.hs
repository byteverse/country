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
  -- * Continent Mapping
  , continent
  -- * Name
  , encodeEnglish
  , decodeEnglish
  -- * Two-letter Codes
  , alphaUpper
  , alphaLower
  , decodeAlpha
  ) where

import Continent.Unsafe

import Control.Monad (forM_)
import Control.Monad.ST (runST)
import Country.Unexposed.Continents (continentAList)
import Country.Unexposed.Util (mapTextArray,charToWord8,word16ToInt,timesTwo)
import Country.Unsafe (Country(Country))
import Data.Char (toLower)
import Data.Text (Text)
import Data.Word (Word8)

import qualified Data.Primitive as Prim
import qualified Data.Text as T
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
    TA.unsafeWrite m ix (charToWord8 a1)
    TA.unsafeWrite m (ix + 1) (charToWord8 a2)
  return m
{-# NOINLINE allAlphaUpper #-}

alphaLower :: Continent -> Text
alphaLower (Continent n) = TI.text allAlphaLower (timesTwo (fromIntegral n)) 2

allAlphaLower :: TA.Array
allAlphaLower = mapTextArray toLower allAlphaUpper
{-# NOINLINE allAlphaLower #-}

decodeAlpha :: Text -> Maybe Continent
decodeAlpha = fmap Continent . flip lookup tbl . T.toUpper
  where tbl = flip map continentNameDb $ \(n,_,(a,b)) -> ((T.toUpper . T.pack) [a,b], n)

encodeEnglish :: Continent -> Text
encodeEnglish (Continent n) = Prim.indexArray englishContinentNamesText (fromIntegral n)

englishContinentNamesText :: Prim.Array Text
englishContinentNamesText = runST $ do
  m <- Prim.newArray numberOfContinents unnamed
  mapM_ (\(ix,name,_) -> Prim.writeArray m (fromIntegral ix) name) continentNameDb
  Prim.unsafeFreezeArray m
{-# NOINLINE englishContinentNamesText #-}

decodeEnglish :: Text -> Maybe Continent
decodeEnglish = fmap Continent . flip lookup tbl
  where tbl = flip map continentNameDb $ \(n,name,_) -> (name, n)

continent :: Country -> Continent
continent (Country n) = Continent $ Prim.indexArray allContinents (word16ToInt n)

allContinents :: Prim.Array Word8
allContinents = runST $ do
  m <- Prim.newArray numberOfPossibleCodes 255
  forM_ continentAList $ \(ix,Continent n) ->
    Prim.writeArray m (word16ToInt ix) n
  Prim.unsafeFreezeArray m
{-# NOINLINE allContinents #-}

unnamed :: Text
unnamed = "Invalid Continent"
{-# NOINLINE unnamed #-}

numberOfPossibleCodes :: Int
numberOfPossibleCodes = 1000
