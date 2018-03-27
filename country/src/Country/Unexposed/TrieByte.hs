module Country.Unexposed.TrieByte
  ( TrieByte
  , trieByteFromList
  , trieByteParser
  ) where

import Data.HashMap.Strict (HashMap)
import Data.Word (Word16,Word8)
import Data.ByteString (ByteString)
import Data.Semigroup (Semigroup)
import Control.Applicative ((<|>))
import qualified Data.ByteString as B
import qualified Data.HashMap.Strict as HM
import qualified Data.Attoparsec.ByteString as AB
import qualified Data.Semigroup as SG

-- | If the value is not the max Word16 (65535), there 
--   is a match. This means that 65535 cannot be used, which 
--   is fine for this since 65535 is not used as a country code.
data TrieByte = TrieByte
  { trieValue :: {-# UNPACK #-} !Word16
  , trieChildren :: !(HashMap Word8 TrieByte)
  }

empty :: TrieByte
empty = TrieByte placeholder HM.empty

append :: TrieByte -> TrieByte -> TrieByte
append (TrieByte v1 c1) (TrieByte v2 c2) = TrieByte (min v1 v2) (HM.unionWith append c1 c2)

placeholder :: Word16
placeholder = 0xFFFF

singleton :: ByteString -> Word16 -> TrieByte
singleton fullName code = go fullName where
  go :: ByteString -> TrieByte
  go name = case B.uncons name of
    Just (char,nameNext) -> TrieByte placeholder (HM.singleton char (go nameNext))
    Nothing -> TrieByte code HM.empty

instance Semigroup TrieByte where
  (<>) = append

instance Monoid TrieByte where
  mempty = empty
  mappend = (SG.<>)

trieByteFromList :: [(ByteString,Word16)] -> TrieByte
trieByteFromList = foldMap (uncurry singleton)

-- it seems like attoparsec should have some kind of convenience
-- for being able to commit to consuming a certain amount of
-- input once your certain that it will be consumed, but I cannot
-- find a way to use the api to do this.
trieByteParser :: TrieByte -> AB.Parser Word16
trieByteParser = go where
  go :: TrieByte -> AB.Parser Word16
  go (TrieByte value children) = do
    let keepGoing = do
          c <- AB.anyWord8
          case HM.lookup c children of
            Nothing -> fail "did not recognize country name"
            Just trieNext -> go trieNext
    if value == placeholder
      then keepGoing
      else keepGoing <|> return value


