{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnboxedTuples #-}

module Country.Subdivision
  ( Subdivision
  -- * Accessors
  , encodeAlpha
  , encodeAlphaShort
  , encodeEnglish
  , encodeEnglishShort
  , category
  -- * Decoding
  , decodeAlpha
  , decodeEnglish
  , decodeEnglishUtf8Bytes
  ) where

import Data.Bytes (Bytes)
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import Data.Primitive.Contiguous (index)
import Data.Primitive.Types (Prim)
import Data.Text (Text)
import Data.Text.Short (ShortText)
import Data.Text.Encoding (encodeUtf8)
import Data.Word (Word16)
import Foreign.Storable (Storable)

import qualified Country.Unexposed.Subdivision as Arrays
import qualified Data.Bytes.HashMap.Word as BytesHashMap
import qualified Data.ByteString as ByteString
import qualified Data.HashMap.Strict as HM
import qualified Data.Primitive.Contiguous as Arr
import qualified Data.Text as T
import qualified GHC.Exts as Exts

newtype Subdivision = Subdivision Word16
  deriving (Eq,Ord,Prim,Hashable,Storable)

instance Show Subdivision where
  show = show . encodeAlpha

instance Enum Subdivision where
  fromEnum (Subdivision w) = fromIntegral w
  toEnum number = if number >= 0 && number < Arrays.actualNumberOfSubdivisions
    then Subdivision (fromIntegral number)
    else error ("toEnum: cannot convert " ++ show number ++ " to Subdivision")
instance Bounded Subdivision where
  minBound = Subdivision 0
  maxBound = Subdivision (fromIntegral $ Arrays.actualNumberOfSubdivisions - 1)


-- country :: Subdivision -> Country
-- country (Subdivision i) = index Arrays.countryArray i

encodeAlpha :: Subdivision -> Text
encodeAlpha (Subdivision i) = index Arrays.codeArray (fromIntegral @Word16 @Int i)

encodeAlphaShort :: Subdivision -> ShortText
encodeAlphaShort (Subdivision i) = index Arrays.codeArrayShort (fromIntegral @Word16 @Int i)

encodeEnglish :: Subdivision -> Text
encodeEnglish (Subdivision i) = index Arrays.nameArray (fromIntegral @Word16 @Int i)

encodeEnglishShort :: Subdivision -> ShortText
encodeEnglishShort (Subdivision i) = index Arrays.nameArrayShort (fromIntegral @Word16 @Int i)

category :: Subdivision -> Text
category (Subdivision i) = index Arrays.categoryArray (fromIntegral @Word16 @Int i)


-- | Decode a 'Subdivision' using its ISO subdivision code.
decodeAlpha :: Text -> Maybe Subdivision
decodeAlpha = flip HM.lookup alphaHashMap

alphaHashMap :: HashMap Text Subdivision
alphaHashMap = Arr.ifoldl'
  (\hm i x ->
      HM.insert x (Subdivision $ fromIntegral i)
    -- $ HM.insert (T.pack [toLower c1, toLower c2, toLower c3]) (Country countryNum)
    $ hm
  )
  HM.empty Arrays.codeArray
{-# NOINLINE alphaHashMap #-}

-- | Decode a 'Subdivision' using its ISO subdivision English name
-- It's not terribly forgiving, accepting only the official(?) names I found on wiki.
decodeEnglish :: Text -> Maybe Subdivision
decodeEnglish = flip HM.lookup englishHashMap

englishHashMap :: HashMap Text Subdivision
englishHashMap = Arr.ifoldl'
  (\hm i x ->
    let place = Subdivision (fromIntegral i)
     in HM.insert x place
      $ HM.insert (T.toLower $ x) place
      $ hm
  )
  HM.empty Arrays.nameArray
{-# NOINLINE englishHashMap #-}

englishHashMapUtf8Bytes :: BytesHashMap.Map
{-# NOINLINE englishHashMapUtf8Bytes #-}
englishHashMapUtf8Bytes = BytesHashMap.fromTrustedList
  ( imap
    (\i t -> (Exts.fromList (ByteString.unpack (encodeUtf8 t)),fromIntegral i)
    ) (Exts.toList Arrays.nameArray)
  )

decodeEnglishUtf8Bytes :: Bytes -> Maybe Subdivision
decodeEnglishUtf8Bytes !bs = case BytesHashMap.lookup bs englishHashMapUtf8Bytes of
  Nothing -> Nothing
  Just w -> Just (Subdivision (fromIntegral w))

imap :: (Int -> a -> b) -> [a] -> [b]
imap f ls = go 0 ls
  where
    go !i (x:xs) = f i x : go (i + 1) xs
    go !_ []     = []
