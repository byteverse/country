{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}

module Country
  ( Country
    -- * Three digit code
  , encodeNumeric
  , decodeNumeric
    -- * Name
  , encodeEnglish
  , decode
    -- * Alpha-2 and Alpha-3
  , alphaTwoUpper
  , alphaThreeUpper
  , alphaThreeLower
  , alphaTwoLower
  , decodeAlphaTwo
  , decodeAlphaThree
  ) where

import Country.Unsafe (Country(..))
import Country.Unexposed.Encode.English (countryNameQuads)
import Country.Unexposed.ExtraNames (extraNames)
import Country.Unexposed.Names (englishCountryNamesText,numberOfPossibleCodes)
import Country.Unexposed.Enumerate (enumeratedCountries)
import Data.Text (Text)
import Data.ByteString (ByteString)
import Data.Word (Word16,Word8)
import Data.Primitive (indexArray,newArray,unsafeFreezeArray,writeArray,
  writeByteArray,indexByteArray,unsafeFreezeByteArray,newByteArray)
import Data.HashMap.Strict (HashMap)
import Data.Primitive.Array (Array(..))
import Data.Primitive.ByteArray (ByteArray(..))
import GHC.Prim (sizeofByteArray#,sizeofArray#)
import GHC.Int (Int(..))
import Control.Monad.ST (runST)
import Control.Monad
import Data.Char (ord,chr,toLower)
import Data.Bits (unsafeShiftL,unsafeShiftR)
import qualified Data.List as L
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.Array as TA
import qualified Data.Text.Internal as TI

-- | Convert a country to its numeric code. This is a
--   three-digit number and will consequently be less than 1000.
encodeNumeric :: Country -> Word16
encodeNumeric (Country n) = n

-- | Get a country from a numeric code. Any code greater than
--   999 will not have a country associated with it. Additionally,
--   many codes are unassigned.
decodeNumeric :: Word16 -> Maybe Country
decodeNumeric n = if n < 1000 && indexByteArray numericValidities (word16ToInt n) == (1 :: Word8)
  then Just (Country n)
  else Nothing

-- | The name of a country given in English
encodeEnglish :: Country -> Text
encodeEnglish (Country n) = indexArray englishCountryNamesText (word16ToInt n)

-- | The alpha-2 country code, uppercase
alphaTwoUpper :: Country -> Text
alphaTwoUpper c = TI.text allAlphaTwoUpper (timesTwo (indexOfCountry c)) 2

-- | The alpha-3 country code, uppercase
alphaThreeUpper :: Country -> Text
alphaThreeUpper c = TI.text allAlphaThreeUpper (timesThree (indexOfCountry c)) 3

-- | The alpha-2 country code, lowercase
alphaTwoLower :: Country -> Text
alphaTwoLower c = TI.text allAlphaTwoLower (timesTwo (indexOfCountry c)) 2

-- | The alpha-3 country code, lowercase
alphaThreeLower :: Country -> Text
alphaThreeLower c = TI.text allAlphaThreeLower (timesThree (indexOfCountry c)) 3

decodeAlphaTwo :: Text -> Maybe Country
decodeAlphaTwo = flip HM.lookup alphaTwoHashMap

decodeAlphaThree :: Text -> Maybe Country
decodeAlphaThree = flip HM.lookup alphaTwoHashMap

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

half :: Int -> Int
half x = unsafeShiftR x 1

timesTwo :: Int -> Int
timesTwo x = unsafeShiftL x 1

timesThree :: Int -> Int
timesThree x = x * 3


-- | Parse a country from its name. This function is language-agnostic.
--   It can handle any source language.
decode :: Text -> Maybe Country
decode = flip HM.lookup decodeMap

word16ToInt :: Word16 -> Int
word16ToInt = fromIntegral

intToWord16 :: Int -> Word16
intToWord16 = fromIntegral

charToWord16 :: Char -> Word16
charToWord16 = fromIntegral . ord

word16ToChar :: Word16 -> Char
word16ToChar = chr . fromIntegral


decodeMap :: HashMap Text Country
decodeMap = 
  let baseMap = HM.union alphaTwoHashMap alphaThreeHashMap
      hm1 = L.foldl' (\hm (country,name) -> HM.insert name country hm) baseMap extraNames
      hm2 = L.foldl' (\hm (countryNum,name,_,_) -> HM.insert name (Country countryNum) hm) hm1 countryNameQuads
   in hm2
{-# NOINLINE decodeMap #-}

arrayFoldl' :: (a -> b -> a) -> a -> Array b -> a
arrayFoldl' f z a = go 0 z
  where
  go i !acc | i < sizeofArray a = go (i+1) (f acc $ indexArray a i)
            | otherwise         = acc

sizeofArray :: Array a -> Int
sizeofArray (Array a) = I# (sizeofArray# a)
{-# INLINE sizeofArray #-}

numberOfCountries :: Int
numberOfCountries = length countryNameQuads

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

-- | The elements in this array are Word16
positions :: ByteArray
positions = runST $ do
  m <- newByteArray (timesTwo numberOfPossibleCodes)
  forM_ (zip (enumFrom (0 :: Word16)) countryNameQuads) $ \(ix,(n,_,_,_)) -> do
    writeByteArray m (word16ToInt n) ix
  unsafeFreezeByteArray m
{-# NOINLINE positions #-}

-- get the index of the country. this refers not to the
-- country code but to the position it shows up in the
-- hard-coded list of all the countries.
indexOfCountry :: Country -> Int
indexOfCountry (Country n) =
  word16ToInt (indexByteArray positions (word16ToInt n))

allAlphaTwoUpper :: TA.Array
allAlphaTwoUpper = TA.run $ do
  m <- TA.new (timesTwo numberOfCountries)
  forM_ countryNameQuads $ \(n,_,(a1,a2),_) -> do
    let ix = timesTwo (indexOfCountry (Country n))
    TA.unsafeWrite m ix (charToWord16 a1)
    TA.unsafeWrite m (ix + 1) (charToWord16 a2)
  return m
{-# NOINLINE allAlphaTwoUpper #-}

allAlphaThreeUpper :: TA.Array
allAlphaThreeUpper = TA.run $ do
  m <- TA.new (timesThree numberOfCountries)
  forM_ countryNameQuads $ \(n,_,_,(a1,a2,a3)) -> do
    let ix = timesThree (indexOfCountry (Country n))
    TA.unsafeWrite m ix (charToWord16 a1)
    TA.unsafeWrite m (ix + 1) (charToWord16 a2)
    TA.unsafeWrite m (ix + 2) (charToWord16 a3)
  return m
{-# NOINLINE allAlphaThreeUpper #-}

allAlphaThreeLower :: TA.Array
allAlphaThreeLower = mapTextArray toLower allAlphaThreeUpper
{-# NOINLINE allAlphaThreeLower #-}

allAlphaTwoLower :: TA.Array
allAlphaTwoLower = mapTextArray toLower allAlphaTwoUpper
{-# NOINLINE allAlphaTwoLower #-}

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


