{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnboxedTuples #-}

module Country.Subdivision
  ( Subdivision
  -- , country -- TODO
  -- * Accessors
  , code
  , name
  , category
  -- * Decoding
  , decodeCode
  , decodeName
  ) where

import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import Data.Primitive.Contiguous (index)
import Data.Primitive.Types (Prim)
import Data.Text.Short (ShortText)
import Data.Word (Word16)
import Foreign.Storable (Storable)

import qualified Country.Unexposed.Subdivision as Arrays
import qualified Data.HashMap.Strict as HM
import qualified Data.Primitive.Contiguous as Arr
import qualified Data.Text as T
import qualified Data.Text.Short as TS

newtype Subdivision = Subdivision Word16
  deriving (Eq,Ord,Prim,Hashable,Storable)

instance Show Subdivision where
  show = show . code

-- country :: Subdivision -> Country
-- country (Subdivision i) = index Arrays.countryArray i

code :: Subdivision -> ShortText
code (Subdivision i) = index Arrays.codeArray (fromIntegral @Word16 @Int i)

name :: Subdivision -> ShortText
name (Subdivision i) = index Arrays.nameArray (fromIntegral @Word16 @Int i)

category :: Subdivision -> ShortText
category (Subdivision i) = index Arrays.categoryArray (fromIntegral @Word16 @Int i)


-- | Decode a 'Subdivision' using its ISO subdivision code.
decodeCode :: ShortText -> Maybe Subdivision
decodeCode = flip HM.lookup codeHashMap

codeHashMap :: HashMap ShortText Subdivision
codeHashMap = Arr.ifoldl'
  (\hm i x ->
      HM.insert x (Subdivision $ fromIntegral i)
    -- $ HM.insert (T.pack [toLower c1, toLower c2, toLower c3]) (Country countryNum)
    $ hm
  )
  HM.empty Arrays.codeArray
{-# NOINLINE codeHashMap #-}

-- | Decode a 'Subdivision' using its ISO subdivision name
-- It's not terribly forgiving, accepting only the official(?) names I found on wiki.
decodeName :: ShortText -> Maybe Subdivision
decodeName = flip HM.lookup nameHashMap

nameHashMap :: HashMap ShortText Subdivision
nameHashMap = Arr.ifoldl'
  (\hm i x ->
    let place = Subdivision (fromIntegral i)
     in HM.insert x place
      $ HM.insert (TS.fromText . T.toLower . TS.toText $ x) place
      $ hm
  )
  HM.empty Arrays.nameArray
{-# NOINLINE nameHashMap #-}
