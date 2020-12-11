{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}

{-# OPTIONS_GHC -Wall #-}

-- | Country type and helpers.
module Country
  ( Country
    -- * Three digit code
  , encodeNumeric
  , decodeNumeric
    -- * Name
  , encodeEnglish
  , decode
  , decodeUtf8
  , decodeUtf8Bytes
  , parser
  , parserUtf8
    -- * Alpha-2 and Alpha-3
  , alphaTwoUpper
  , alphaTwoUpperUtf8Ptr
  , alphaTwoUpperUtf8BoundedBuilder
  , alphaThreeUpper
  , alphaThreeLower
  , alphaTwoLower
  , decodeAlphaTwo
  , decodeAlphaThree
    -- * Hash Maps for Decoding
  , hashMapUtf8
  , hashMapUtf16
  ) where

import Control.Monad (forM_)
import Control.Monad.ST (runST)
import Country.Unexposed.AlphaTwoPtr (alphaTwoPtr)
import Country.Unexposed.Encode.English (countryNameQuads)
import Country.Unexposed.Names (hashMapUtf16,hashMapUtf8)
import Country.Unexposed.Names (numberOfPossibleCodes,alphaTwoHashMap,alphaThreeHashMap,decodeMap,decodeMapUtf8,decodeNumeric,encodeEnglish)
import Country.Unexposed.Trie (Trie,trieFromList,trieParser)
import Country.Unexposed.TrieByte (TrieByte,trieByteFromList,trieByteParser)
import Country.Unexposed.Util (mapTextArray,charToWord16,word16ToInt,timesTwo,timesThree)
import Country.Unsafe (Country(..))
import Data.Bytes.Types (Bytes(Bytes))
import Data.ByteString (ByteString)
import Data.Char (toLower)
import Data.Coerce (coerce)
import Data.Primitive (writeByteArray,indexByteArray,unsafeFreezeByteArray,newByteArray)
import Data.Primitive.ByteArray (ByteArray(..))
import Data.Primitive.Ptr (indexOffPtr)
import Data.Text (Text)
import Data.Word (Word16)
import Data.Word (Word8)
import Foreign.Ptr (Ptr,plusPtr)

import qualified Data.Attoparsec.ByteString as AB
import qualified Data.Attoparsec.Text as AT
import qualified Data.Bytes.Builder.Bounded.Unsafe as BBU
import qualified Data.Bytes.HashMap.Word as BytesHashMap
import qualified Data.HashMap.Strict as HM
import qualified Data.Text.Array as TA
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Internal as TI

-- | Convert a country to its numeric code. This is a
--   three-digit number and will consequently be less than 1000.
encodeNumeric :: Country -> Word16
encodeNumeric (Country n) = n

-- | The alpha-2 country code, uppercase
alphaTwoUpper :: Country -> Text
alphaTwoUpper c = TI.text allAlphaTwoUpper (timesTwo (indexOfCountry c)) 2

-- | The alpha-2 country code, uppercase. The resulting address always
-- has two bytes at it.
alphaTwoUpperUtf8Ptr :: Country -> Ptr Word8
alphaTwoUpperUtf8Ptr (Country c) =
  plusPtr alphaTwoPtr (2 * fromIntegral c)

alphaTwoUpperUtf8BoundedBuilder :: Country -> BBU.Builder 2
alphaTwoUpperUtf8BoundedBuilder !c = BBU.construct
  (\arr ix -> do
    let ptr = alphaTwoUpperUtf8Ptr c
    writeByteArray arr ix (indexOffPtr ptr 0)
    writeByteArray arr (ix + 1) (indexOffPtr ptr 1)
    pure (ix + 2)
  )

-- | The alpha-3 country code, uppercase
alphaThreeUpper :: Country -> Text
alphaThreeUpper c = TI.text allAlphaThreeUpper (timesThree (indexOfCountry c)) 3

-- | The alpha-2 country code, lowercase
alphaTwoLower :: Country -> Text
alphaTwoLower c = TI.text allAlphaTwoLower (timesTwo (indexOfCountry c)) 2

-- | The alpha-3 country code, lowercase
alphaThreeLower :: Country -> Text
alphaThreeLower c = TI.text allAlphaThreeLower (timesThree (indexOfCountry c)) 3

-- | Decode a 'Country' using its alpha-2 country code.
decodeAlphaTwo :: Text -> Maybe Country
decodeAlphaTwo = flip HM.lookup alphaTwoHashMap

-- | Decode a 'Country' using its alpha-3 country code.
decodeAlphaThree :: Text -> Maybe Country
decodeAlphaThree = flip HM.lookup alphaThreeHashMap


-- | Parse a country from its name. This function is language-agnostic
--   and is very generous with what it accepts. It handles official
--   names, colloquial names, acroynms, and obsolete names for many
--   countries. It strives to handle any source language. Open an
--   issue on the issue tracker if there are names that are missing.
decode :: Text -> Maybe Country
decode (TI.Text (TA.Array arr) off16 len16) =
  case (BytesHashMap.lookup (Bytes (ByteArray arr) (off16 * 2) (len16 * 2)) hashMapUtf16) of
    Nothing -> Nothing
    Just w -> Just (Country (fromIntegral w))

-- | Decode a 'Country' from a UTF-8-encoded 'ByteString'.
decodeUtf8 :: ByteString -> Maybe Country
decodeUtf8 = flip HM.lookup decodeMapUtf8

decodeUtf8Bytes :: Bytes -> Maybe Country
decodeUtf8Bytes !bs = case (BytesHashMap.lookup bs hashMapUtf8) of
  Nothing -> Nothing
  Just w -> Just (Country (fromIntegral w))

-- | Parse a country from its name using an attoparsec text parser. This
--   function is language-agnostic and can handle any source language.
--   In the case that one possible country name is a prefix of another
--   possible name (for example, United States vs United States of America),
--   the longest possible will be parsed.
parser :: AT.Parser Country
parser = coerce (trieParser decodeTrie)

-- | Parse a 'Country' using an 'AB.Parser'.
parserUtf8 :: AB.Parser Country
parserUtf8 = coerce (trieByteParser decodeTrieUtf8)

numberOfCountries :: Int
numberOfCountries = length countryNameQuads

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

decodeTrie :: Trie
decodeTrie = trieFromList (map (\(a,Country x) -> (a,x)) (HM.toList decodeMap))
{-# NOINLINE decodeTrie #-}

decodeTrieUtf8 :: TrieByte
decodeTrieUtf8 = trieByteFromList (map (\(a,Country x) -> (TE.encodeUtf8 a,x)) (HM.toList decodeMap))
{-# NOINLINE decodeTrieUtf8 #-}
