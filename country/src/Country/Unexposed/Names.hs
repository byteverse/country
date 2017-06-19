module Country.Unexposed.Names
  ( englishCountryNamesText
  , englishIdentifierNamesText
  , numberOfPossibleCodes
  ) where

import Control.Monad.ST
import Data.Text (Text)
import Data.Word
import Data.Char (toLower,isAlpha)
import Country.Unexposed.Encode.English (countryNameQuads)
import Data.Primitive (Array,indexArray,newArray,unsafeFreezeArray,writeArray,
  writeByteArray,indexByteArray,unsafeFreezeByteArray,newByteArray)
import qualified Data.Text as T

englishCountryNamesText :: Array Text
englishCountryNamesText = runST $ do
  m <- newArray numberOfPossibleCodes unnamed
  mapM_ (\(ix,name,_,_) -> writeArray m (word16ToInt ix) name) countryNameQuads
  unsafeFreezeArray m
{-# NOINLINE englishCountryNamesText #-}

englishIdentifierNamesText :: Array Text
englishIdentifierNamesText = runST $ do
  m <- newArray numberOfPossibleCodes unnamed
  mapM_ (\(ix,name,_,_) -> writeArray m (word16ToInt ix) (toIdentifier name)) countryNameQuads
  unsafeFreezeArray m
{-# NOINLINE englishIdentifierNamesText #-}

toIdentifier :: Text -> Text
toIdentifier t = case (T.uncons . T.filter isAlpha . T.toTitle) t of
  Nothing -> T.empty
  Just (b,bs) -> T.cons (toLower b) bs


unnamed :: Text
unnamed = T.pack "Invalid Country"
{-# NOINLINE unnamed #-}

numberOfPossibleCodes :: Int
numberOfPossibleCodes = 1000

word16ToInt :: Word16 -> Int
word16ToInt = fromIntegral

