{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Continent.Unsafe
  ( Continent(..)
  , pattern Africa
  , pattern Asia
  , pattern Antarctica
  , pattern Europe
  , pattern NorthAmerica
  , pattern Oceania
  , pattern SouthAmerica
  , continentNameDb
  ) where

import Data.Text (Text)
import Data.Word (Word8)


newtype Continent = Continent Word8
  deriving(Eq,Ord,Enum)


{-# COMPLETE Africa, Asia, Antarctica, Europe, NorthAmerica, Oceania, SouthAmerica #-}
pattern Africa, Asia, Antarctica, Europe, NorthAmerica, Oceania, SouthAmerica :: Continent
pattern Africa = Continent 0
pattern Asia = Continent 1
pattern Antarctica = Continent 2
pattern Europe = Continent 3
pattern NorthAmerica = Continent 4
pattern Oceania = Continent 5
pattern SouthAmerica = Continent 6


continentNameDb :: [(Word8,Text,(Char,Char))]
continentNameDb =
  [ (0, "Africa", ('A', 'F'))
  , (1, "Asia", ('A', 'N'))
  , (2, "Antarctica", ('A', 'S'))
  , (3, "Europe", ('E', 'U'))
  , (4, "North america", ('N', 'A'))
  , (5, "Oceania", ('O', 'C'))
  , (6, "South america", ('S', 'A'))
  ]

instance Bounded Continent where
  minBound = Continent 0
  maxBound = Continent 6

instance Show Continent where
  show Africa = "Africa"
  show Asia = "Asia"
  show Antarctica = "Antarctica"
  show Europe = "Europe"
  show NorthAmerica = "NorthAmerica"
  show Oceania = "Oceania"
  show SouthAmerica = "SouthAmerica"
  show (Continent n) = "Continent " ++ show n
