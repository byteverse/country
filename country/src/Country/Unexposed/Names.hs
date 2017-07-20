{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# OPTIONS_HADDOCK not-home #-}

module Country.Unexposed.Names
  ( englishCountryNamesText
  , englishIdentifierNamesText
  , numberOfPossibleCodes
  , decodeMap
  , alphaTwoHashMap
  , alphaThreeHashMap
  , decodeNumeric
  , encodeEnglish
  , Country(..)
  ) where

import Data.Word (Word16)
import Data.Hashable (Hashable)
import Data.Primitive.Types (Prim)
import Data.HashMap.Strict (HashMap)
import Data.Primitive (indexArray)
import Data.Primitive.Array (Array(..))
import Data.Primitive.ByteArray (ByteArray(..))
import Control.Monad
import qualified Data.Text as T
import qualified Data.Aeson as AE
import qualified Data.Aeson.Types as AET
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import Control.Monad.ST
import Data.Text (Text)
import Data.Word
import Data.Char (toLower,isAlpha)
import Country.Unexposed.Encode.English (countryNameQuads)
import Data.Primitive (Array,indexArray,newArray,unsafeFreezeArray,writeArray,
  writeByteArray,indexByteArray,unsafeFreezeByteArray,newByteArray)
import qualified Data.Text as T
import qualified Data.Scientific as SCI

-- | The name of a country given in English
encodeEnglish :: Country -> Text
encodeEnglish (Country n) = indexArray englishCountryNamesText (word16ToInt n)

mexico :: Country
mexico = Country 484

unitedStatesOfAmerica :: Country
unitedStatesOfAmerica = Country 840

ålandIslands :: Country
ålandIslands = Country 248

venezuelaBolivarianRepublicOf :: Country
venezuelaBolivarianRepublicOf = Country 862

boliviaPlurinationalStateOf :: Country
boliviaPlurinationalStateOf = Country 68

extraNames :: [(Country,Text)]
extraNames =
  [ (unitedStatesOfAmerica,"United States")
  , (unitedStatesOfAmerica,"The United States")
  , (unitedStatesOfAmerica,"USA")
  , (unitedStatesOfAmerica,"U.S.A.")
  , (unitedStatesOfAmerica,"Estados Unidos de América")
  , (mexico,"Estados Unidos Mexicanos")
  , (mexico,"México")
  , (mexico,"Méjico")
  , (ålandIslands,"Aland Islands")
  , (ålandIslands,"Aaland Islands")
  , (venezuelaBolivarianRepublicOf,"Venezuela")
  , (venezuelaBolivarianRepublicOf,"Bolivarian Republic of Venezuela")
  , (boliviaPlurinationalStateOf,"Bolivia")
  , (boliviaPlurinationalStateOf,"Plurinational State of Bolivia")
  ]

englishCountryNamesText :: Array Text
englishCountryNamesText = runST $ do
  m <- newArray numberOfPossibleCodes unnamed
  mapM_ (\(ix,name,_,_) -> writeArray m (word16ToInt ix) name) countryNameQuads
  unsafeFreezeArray m
{-# NOINLINE englishCountryNamesText #-}

englishIdentifierNamesText :: Array Text
englishIdentifierNamesText = runST $ do
  m <- newArray numberOfPossibleCodes unnamed
  mapM_ (\(ix,name,_,_) -> writeArray m (word16ToInt ix) (toIdentifier name)) countryNameQuads
  unsafeFreezeArray m
{-# NOINLINE englishIdentifierNamesText #-}

toIdentifier :: Text -> Text
toIdentifier t = case (T.uncons . T.filter isAlpha . T.toTitle) t of
  Nothing -> T.empty
  Just (b,bs) -> T.cons (toLower b) bs


unnamed :: Text
unnamed = T.pack "Invalid Country"
{-# NOINLINE unnamed #-}

numberOfPossibleCodes :: Int
numberOfPossibleCodes = 1000

word16ToInt :: Word16 -> Int
word16ToInt = fromIntegral

decodeMap :: HashMap Text Country
decodeMap = 
  let baseMap = HM.union alphaTwoHashMap alphaThreeHashMap
      hm1 = L.foldl' (\hm (country,name) -> HM.insert name country hm) baseMap extraNames
      hm2 = L.foldl' (\hm (countryNum,name,_,_) -> HM.insert name (Country countryNum) hm) hm1 countryNameQuads
   in hm2
{-# NOINLINE decodeMap #-}

-- | A country recognized by ISO 3166.
newtype Country = Country Word16
  deriving (Eq,Ord,Prim,Hashable)

instance Show Country where
  show (Country n) = T.unpack (indexArray englishIdentifierNamesText (word16ToInt n))

-- todo: add support for encoding directly to bytestring.
-- Also, add suport for ToJSONKey and FromJSONKey once everything
-- finally gets off of aeson-0.11 (looking at you, reflex-platform)
instance AE.ToJSON Country where
  toJSON = AET.String . encodeEnglish

instance AE.FromJSON Country where
  parseJSON x = case x of
    AET.String t -> case HM.lookup t decodeMap of
      Nothing -> fail $ "invalid country name " ++ T.unpack t
      Just country -> return country
    AET.Number n -> case SCI.toBoundedInteger n of
      Nothing -> fail errMsg
      Just w -> case decodeNumeric w of
        Just c -> return c
        Nothing -> fail errMsg
      where errMsg = fail $ "invalid country code " ++ show n
    _ -> AET.typeMismatch "Country" x

-- | Get a country from a numeric code. Any code greater than
--   999 will not have a country associated with it. Additionally,
--   many codes are unassigned.
decodeNumeric :: Word16 -> Maybe Country
decodeNumeric n = if n < 1000 && indexByteArray numericValidities (word16ToInt n) == (1 :: Word8)
  then Just (Country n)
  else Nothing

-- | The elements in this array are Word8 (basically boolean)
numericValidities :: ByteArray
numericValidities = runST $ do
  m <- newByteArray numberOfPossibleCodes
  let clear !ix = if ix < numberOfPossibleCodes
        then writeByteArray m ix (0 :: Word8)
        else return ()
  clear 0
  forM_ countryNameQuads $ \(n,_,_,_) -> do
    writeByteArray m (word16ToInt n) (1 :: Word8)
  unsafeFreezeByteArray m
{-# NOINLINE numericValidities #-}

alphaTwoHashMap :: HashMap Text Country
alphaTwoHashMap = L.foldl'
  (\hm (countryNum,_,(c1,c2),_) ->
      HM.insert (T.pack [c1,c2]) (Country countryNum)
    $ HM.insert (T.pack [toLower c1, toLower c2]) (Country countryNum)
    $ hm
  )
  HM.empty countryNameQuads
{-# NOINLINE alphaTwoHashMap #-}

alphaThreeHashMap :: HashMap Text Country
alphaThreeHashMap = L.foldl'
  (\hm (countryNum,_,_,(c1,c2,c3)) -> 
      HM.insert (T.pack [c1,c2,c3]) (Country countryNum)
    $ HM.insert (T.pack [toLower c1, toLower c2, toLower c3]) (Country countryNum)
    $ hm
  )
  HM.empty countryNameQuads
{-# NOINLINE alphaThreeHashMap #-}

