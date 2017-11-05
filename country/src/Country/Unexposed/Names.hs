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
  , decodeMapUtf8
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
import Data.ByteString (ByteString)
import Data.Primitive (indexArray)
import Data.Primitive.Array (Array(..))
import Data.Primitive.ByteArray (ByteArray(..))
import Control.Monad
import Data.Text.Encoding (encodeUtf8)
import Country.Unexposed.Alias (aliases)
import qualified Data.Text as T
import qualified Data.Aeson as AE
import qualified Data.Aeson.Types as AET
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import Control.Monad.ST
import Foreign.Storable (Storable)
import Data.Text (Text)
import Data.Word
import Data.Char (toLower,isAlpha)
import Country.Unexposed.Encode.English (countryNameQuads)
import Data.Primitive (Array,indexArray,newArray,unsafeFreezeArray,writeArray,
  writeByteArray,indexByteArray,unsafeFreezeByteArray,newByteArray,sizeOf)
import qualified Data.Text as T
import qualified Data.Scientific as SCI

-- | The name of a country given in English
encodeEnglish :: Country -> Text
encodeEnglish (Country n) = indexArray englishCountryNamesText (word16ToInt n)

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
      hm1 = L.foldl' (\hm (countryNum,name) -> HM.insert name (Country countryNum) hm) baseMap aliases
      hm2 = L.foldl' (\hm (countryNum,name,_,_) -> HM.insert name (Country countryNum) hm) hm1 countryNameQuads
      hm3 = HM.foldlWithKey' (\hm name cty -> HM.insert (T.toLower name) cty $ HM.insert (T.toTitle name) cty $ hm) hm2 hm2
   in hm3
{-# NOINLINE decodeMap #-}

decodeMapUtf8 :: HashMap ByteString Country
decodeMapUtf8 = HM.foldlWithKey' (\hm k v -> HM.insert (encodeUtf8 k) v hm) HM.empty decodeMap
{-# NOINLINE decodeMapUtf8 #-}

-- | A country recognized by ISO 3166.
newtype Country = Country Word16
  deriving (Eq,Ord,Prim,Hashable,Storable)

instance Show Country where
  show (Country n) = T.unpack (indexArray englishIdentifierNamesText (word16ToInt n))

instance Enum Country where
  fromEnum (Country w) = indexByteArray countryCodeToSequentialMapping (fromIntegral w)
  toEnum number = if number >= 0 && number < actualNumberOfCountries
    then Country (indexByteArray sequentialToCountryCodeMapping number)
    else error ("toEnum: cannot convert " ++ show number ++ " to Country")

instance Bounded Country where
  minBound = Country (indexByteArray sequentialToCountryCodeMapping 0)
  maxBound = Country (indexByteArray sequentialToCountryCodeMapping (actualNumberOfCountries - 1))

orderedCountryCodes :: [Word16]
orderedCountryCodes = L.sort $ map (\(a,_,_,_) -> a) countryNameQuads

countryCodeToSequentialMapping :: ByteArray
countryCodeToSequentialMapping = runST $ do
  numbers <- newByteArray (numberOfPossibleCodes * sizeOf (undefined :: Int))
  forM_ (zip [0 :: Int,1..] orderedCountryCodes) $ \(number,code) -> do
    writeByteArray numbers (word16ToInt code) number
  unsafeFreezeByteArray numbers
{-# NOINLINE countryCodeToSequentialMapping #-}

sequentialToCountryCodeMapping :: ByteArray
sequentialToCountryCodeMapping = runST $ do
  codes <- newByteArray (actualNumberOfCountries * sizeOf (undefined :: Word16))
  forM_ (zip [0 :: Int,1..] orderedCountryCodes) $ \(number,code) -> do
    writeByteArray codes number (code :: Word16)
  unsafeFreezeByteArray codes
{-# NOINLINE sequentialToCountryCodeMapping #-}

actualNumberOfCountries :: Int
actualNumberOfCountries = length countryNameQuads
{-# NOINLINE actualNumberOfCountries #-}
  

codeToEnum :: Word16 -> Int
codeToEnum w = indexByteArray countryCodeToSequentialMapping (word16ToInt w)

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

