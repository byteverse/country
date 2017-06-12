{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}

module Country
  ( Country
  , encodeNumeric
  , decodeNumeric
  , encodeEnglish
  , decode
  ) where

import Country.Unsafe (Country(..))
import Country.Unexposed.Encode.English (englishCountryNamesText,countryNamePairs)
import Country.Unexposed.ExtraNames (extraNames)
import Country.Unexposed.Enumerate (enumeratedCountries)
import Data.Text (Text)
import Data.ByteString (ByteString)
import Data.Word (Word16)
import Data.Primitive (indexArray)
import Data.HashMap.Strict (HashMap)
import Data.Primitive.Array (Array(..))
import GHC.Prim (sizeofArray#)
import GHC.Int (Int(..))
import qualified Data.List as L
import qualified Data.HashMap.Strict as HM

encodeNumeric :: Country -> Word16
encodeNumeric (Country n) = n

decodeNumeric :: Word16 -> Maybe Country
decodeNumeric n = if n < 1000
  then Just (Country n)
  else Nothing

encodeEnglish :: Country -> Text
encodeEnglish (Country n) = indexArray englishCountryNamesText (word16ToInt n)

-- | The decoding should be as lenient as possible.
decode :: Text -> Maybe Country
decode = flip HM.lookup decodeMap

word16ToInt :: Word16 -> Int
word16ToInt = fromIntegral

intToWord16 :: Int -> Word16
intToWord16 = fromIntegral


decodeMap :: HashMap Text Country
decodeMap = 
  let hm1 = L.foldl' (\hm (country,name) -> HM.insert name country hm) HM.empty extraNames
      hm2 = L.foldl' (\hm (countryNum,name) -> HM.insert name (Country (intToWord16 countryNum)) hm) hm1 countryNamePairs
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
