module Country.Unexposed.Trie
  ( Trie
  , trieFromList
  , trieParser
  ) where

import Data.HashMap.Strict (HashMap)
import Data.Word (Word16)
import Data.Text (Text)
import Control.Applicative ((<|>))
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import qualified Data.Attoparsec.Text as AT

-- | If the value is not the max Word16 (65535), there 
--   is a match. This means that 65535 cannot be used, which 
--   is fine for this since 65535 is not used as a country code.
data Trie = Trie
  { trieValue :: {-# UNPACK #-} !Word16
  , trieChildren :: !(HashMap Char Trie)
  }

empty :: Trie
empty = Trie placeholder HM.empty

append :: Trie -> Trie -> Trie
append (Trie v1 c1) (Trie v2 c2) = Trie (min v1 v2) (HM.unionWith append c1 c2)

placeholder :: Word16
placeholder = 0xFFFF

singleton :: Text -> Word16 -> Trie
singleton fullName code = go fullName where
  go :: Text -> Trie
  go name = case T.uncons name of
    Just (char,nameNext) -> Trie placeholder (HM.singleton char (go nameNext))
    Nothing -> Trie code HM.empty

instance Monoid Trie where
  mempty = empty
  mappend = append

trieFromList :: [(Text,Word16)] -> Trie
trieFromList = foldMap (uncurry singleton)

-- it seems like attoparsec should have some kind of convenience
-- for being able to commit to consuming a certain amount of
-- input once your certain that it will be consumed, but I cannot
-- find a way to use the api to do this.
trieParser :: Trie -> AT.Parser Word16
trieParser = go where
  go :: Trie -> AT.Parser Word16
  go (Trie value children) = do
    let keepGoing = do
          c <- AT.anyChar
          case HM.lookup c children of
            Nothing -> fail "did not recognize country name"
            Just trieNext -> go trieNext
    if value == placeholder
      then keepGoing
      else keepGoing <|> return value

