{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}

module Country
  ( Country
  , encodeNumeric
  , decodeNumeric
  , encodeEnglish
  , decode
  , alphaTwo
  ) where

import Country.Unsafe (Country(..))
import Country.Unexposed.Encode.English (countryNameQuads)
import Country.Unexposed.ExtraNames (extraNames)
import Country.Unexposed.Enumerate (enumeratedCountries)
import Data.Text (Text)
import Data.ByteString (ByteString)
import Data.Word (Word16)
import Data.Primitive (indexArray,newArray,unsafeFreezeArray,writeArray,
  writeByteArray,indexByteArray,unsafeFreezeByteArray,newByteArray)
import Data.HashMap.Strict (HashMap)
import Data.Primitive.Array (Array(..))
import Data.Primitive.ByteArray (ByteArray(..))
import GHC.Prim (sizeofArray#)
import GHC.Int (Int(..))
import Control.Monad.ST (runST)
import Control.Monad
import Data.Char (ord)
import qualified Data.List as L
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.Array as TA
import qualified Data.Text.Internal as TI

encodeNumeric :: Country -> Word16
encodeNumeric (Country n) = n

decodeNumeric :: Word16 -> Maybe Country
decodeNumeric n = if n < 1000
  then Just (Country n)
  else Nothing

encodeEnglish :: Country -> Text
encodeEnglish (Country n) = indexArray englishCountryNamesText (word16ToInt n)

alphaTwo :: Country -> Text
alphaTwo c = TI.text allAlphaTwo (indexOfCountry c) 2

-- | The decoding should be as lenient as possible.
decode :: Text -> Maybe Country
decode = flip HM.lookup decodeMap

word16ToInt :: Word16 -> Int
word16ToInt = fromIntegral

intToWord16 :: Int -> Word16
intToWord16 = fromIntegral

charToWord16 :: Char -> Word16
charToWord16 = fromIntegral . ord


decodeMap :: HashMap Text Country
decodeMap = 
  let hm1 = L.foldl' (\hm (country,name) -> HM.insert name country hm) HM.empty extraNames
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

englishCountryNamesText :: Array Text
englishCountryNamesText = runST $ do
  m <- newArray numberOfPossibleCodes unnamed
  mapM_ (\(ix,name,_,_) -> writeArray m (word16ToInt ix) name) countryNameQuads
  unsafeFreezeArray m
{-# NOINLINE englishCountryNamesText #-}

unnamed :: Text
unnamed = T.pack "Invalid Country"
{-# NOINLINE unnamed #-}

numberOfCountries :: Int
numberOfCountries = length countryNameQuads

numberOfPossibleCodes :: Int
numberOfPossibleCodes = 1000

positions :: ByteArray
positions = runST $ do
  m <- newByteArray (2 * numberOfPossibleCodes)
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

allAlphaTwo :: TA.Array
allAlphaTwo = TA.run $ do
  m <- TA.new (2 * numberOfCountries)
  forM_ countryNameQuads $ \(n,_,(a1,a2),_) -> do
    let ix = indexOfCountry (Country n)
    TA.unsafeWrite m ix (charToWord16 a1)
    TA.unsafeWrite m (ix + 1) (charToWord16 a2)
  return m
{-# NOINLINE allAlphaTwo #-}

