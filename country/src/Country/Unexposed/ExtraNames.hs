{-# LANGUAGE OverloadedStrings #-}

module Country.Unexposed.ExtraNames
  ( extraNames
  ) where

import Country.Unsafe (Country)
import Country.Identifier
import Data.Text (Text)
import Data.Primitive (indexArray,newArray,unsafeFreezeArray,writeArray,
  writeByteArray,indexByteArray,unsafeFreezeByteArray,newByteArray)

extraNames :: [(Country,Text)]
extraNames =
  [ (unitedStatesOfAmerica,"United States")
  , (unitedStatesOfAmerica,"USA")
  , (unitedStatesOfAmerica,"U.S.A.")
  , (mexico,"Estados Unidos Mexicanos")
  , (mexico,"México")
  , (mexico,"Méjico")
  ]

