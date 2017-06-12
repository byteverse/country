module Country
  ( Country
  , encodeNumeric
  , decodeNumeric
  ) where

import Country.Unsafe (Country(..))
import Data.Text (Text)
import Data.ByteString (ByteString)

encodeNumeric :: Country -> Word16
encodeNumeric (Country n) = n

decodeNumeric :: Word16 -> Maybe Country
decodeNumeric n = if n < 1000
  then Just (Country n)
  else Nothing



