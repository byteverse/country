{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_HADDOCK not-home #-}

{-| This module provides the data constructor for a 'Country'.
    While pattern matching on a country is perfectly safe,
    constructing one is not. There is an invariant the type
    system does not capture that the country number, as defined
    by ISO 3166-1, is between the inclusive bounds 0 and 999.
    Failure to maintain this invariant can cause other functions
    in this library to segfault.
-}
module Country.Unsafe
  ( Country(..)
  ) where

import Data.Word (Word16)
import Data.Hashable (Hashable)
import Data.Primitive.Types (Prim)
import Data.Primitive (indexArray)
import Country.Unexposed.Names (englishIdentifierNamesText)
import qualified Data.Text as T

-- | A country recognized by ISO 3166.
newtype Country = Country Word16
  deriving (Eq,Ord,Prim,Hashable)

instance Show Country where
  show (Country n) = T.unpack (indexArray englishIdentifierNamesText (word16ToInt n))

word16ToInt :: Word16 -> Int
word16ToInt = fromIntegral

