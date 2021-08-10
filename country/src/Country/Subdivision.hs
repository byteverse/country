{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnboxedTuples #-}

module Country.Subdivision
  ( Subdivision
  -- , country -- TODO
  , code
  , name
  , category
  ) where

import Data.Hashable (Hashable)
import Data.Primitive.Contiguous (index)
import Data.Primitive.Types (Prim)
import Data.Text.Short (ShortText)
import Data.Word (Word16)
import Foreign.Storable (Storable)

import qualified Country.Unexposed.Subdivision as Arrays

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
