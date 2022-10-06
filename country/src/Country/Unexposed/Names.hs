{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}

{-# OPTIONS_HADDOCK not-home #-}

module Country.Unexposed.Names
  ( englishCountryNamesText
  , englishIdentifierNamesText
  , numberOfPossibleCodes
  , decodeMap
  , hashMapUtf8
  , hashMapUtf16
  , decodeMapUtf8
  , alphaTwoHashMap
  , alphaThreeHashMap
  , decodeNumeric
  , encodeEnglish
  , encodeEnglishShort
  , Country(..)
  ) where

import Control.Monad
import Control.Monad.ST
import Data.Word

import Control.DeepSeq (NFData)
import Country.Unexposed.Alias (aliases)
import Country.Unexposed.Encode.English (countryNameQuads)
import Data.Bytes.Types (Bytes(Bytes))
import Data.ByteString (ByteString)
import Data.Char (toLower,isAlpha,toUpper)
import Data.Data
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import Data.Primitive (Array,indexArray,newArray,unsafeFreezeArray,writeArray)
import Data.Primitive (sizeOf)
import Data.Primitive (writeByteArray,indexByteArray,unsafeFreezeByteArray,newByteArray)
import Data.Primitive.ByteArray (ByteArray(..))
import Data.Primitive.Types (Prim)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8,encodeUtf16BE)
import Data.Text.Short (ShortText)
import Foreign.Storable (Storable)
import GHC.Generics (Generic)

import qualified Data.Aeson as AE
import qualified Data.Aeson.Types as AET
import qualified Data.Bytes as Bytes
import qualified Data.Bytes.HashMap.Word as BytesHashMap
import qualified Data.ByteString as ByteString
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import qualified Data.Primitive.Unlifted.Array as PM
import qualified Data.Scientific as SCI
import qualified Data.Text as T
import qualified Data.Text.Array as Text
import qualified Data.Text.Internal as Text
import qualified Data.Text.Short as TS
import qualified GHC.Exts as Exts

-- | The name of a country given in English
encodeEnglish :: Country -> Text
{-# inline encodeEnglish #-}
encodeEnglish (Country n) = indexArray englishCountryNamesText (word16ToInt n)

-- | The name of a country given in English
encodeEnglishShort :: Country -> ShortText
{-# inline encodeEnglishShort #-}
encodeEnglishShort (Country n) =
  PM.indexUnliftedArray englishCountryNamesShortText (word16ToInt n)

englishCountryNamesShortText :: PM.UnliftedArray ShortText
englishCountryNamesShortText = runST $ do
  m <- PM.newUnliftedArray numberOfPossibleCodes unnamedShort
  mapM_ (\(ix,name,_,_) -> PM.writeUnliftedArray m (word16ToInt ix) (TS.fromText name)) countryNameQuads
  PM.unsafeFreezeUnliftedArray m
{-# NOINLINE englishCountryNamesShortText #-}

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
toIdentifier t = case (T.uncons . T.filter isAlpha . slowToTitle) t of
  Nothing -> T.empty
  Just (b,bs) -> T.cons (toLower b) bs


unnamed :: Text
unnamed = T.pack "Invalid Country"
{-# NOINLINE unnamed #-}

unnamedShort :: ShortText
unnamedShort = TS.pack "Invalid Country"
{-# NOINLINE unnamedShort #-}

numberOfPossibleCodes :: Int
numberOfPossibleCodes = 1000

word16ToInt :: Word16 -> Int
word16ToInt = fromIntegral

decodeMap :: HashMap Text Country
{-# NOINLINE decodeMap #-}
decodeMap = HM.fromList (map (\(a,b) -> (b,Country a)) countryPairs)

hashMapUtf8 :: BytesHashMap.Map
hashMapUtf8 = BytesHashMap.fromTrustedList
  ( map
    (\(a,t) -> (Exts.fromList (ByteString.unpack (encodeUtf8 t)),fromIntegral a)
    ) countryPairs
  )

-- It is a hack to pull from a source of randomness in here, but whatever.
-- Maybe I can get rid of this if GHC ever supports casing on values of
-- type ByteArray# along with good codegen for it.
hashMapUtf16 :: BytesHashMap.Map
hashMapUtf16 = BytesHashMap.fromTrustedList
  ( map
    (\(a, str) ->
      (Bytes.fromByteString $ encodeUtf16BE str, fromIntegral a)
    ) countryPairs
  )
countryPairs :: [(Word16,Text)]
{-# NOINLINE countryPairs #-}
countryPairs =
  let x = aliases ++ concatMap
        (\(num,name,(c2a,c2b),(c3a,c3b,c3c)) ->
          [ (num,name)
          , (num,T.pack [c2a,c2b])
          , (num,T.pack [c3a,c3b,c3c])
          , (num,T.pack [toLower c2a,toLower c2b])
          , (num,T.pack [toLower c3a,toLower c3b,toLower c3c])
          ]
        ) countryNameQuads
   in x ++ map (\(a,b) -> (a,slowToTitle b)) x

-- This is only needed to support the reflex-platform fork of text. Fortunately,
-- in all the places this is needed, it is only called to build CAFs.
slowToTitle :: Text -> Text
slowToTitle = T.intercalate (T.singleton ' ') . map upperFirst . T.splitOn (T.singleton ' ')

upperFirst :: Text -> Text
upperFirst t = case T.uncons t of
  Nothing -> T.empty
  Just (c,cs) -> T.cons (toUpper c) cs

decodeMapUtf8 :: HashMap ByteString Country
decodeMapUtf8 = HM.foldlWithKey' (\hm k v -> HM.insert (encodeUtf8 k) v hm) HM.empty decodeMap
{-# NOINLINE decodeMapUtf8 #-}

-- | A country recognized by ISO 3166.
newtype Country = Country Word16
  deriving (Eq,Ord,Prim,Hashable,Storable,NFData,Generic,Data,Typeable)

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
