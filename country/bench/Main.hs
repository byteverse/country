{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}

import Gauge.Main (defaultMain,bgroup,bench,whnf)
import Data.Text (Text)

import qualified Data.Bytes as Bytes
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Country

main :: IO ()
main = defaultMain
  [ bench "decode" (whnf decodeManyCountries ex1)
  ]

ex1 :: Text
{-# noinline ex1 #-}
ex1 = Text.intercalate
  (Text.singleton '\n')
  (List.map Country.encodeEnglish [minBound..maxBound])

decodeManyCountries :: Text -> Word
{-# noinline decodeManyCountries #-}
decodeManyCountries txt = List.foldl'
  (\acc t -> acc + maybe 0 (fromIntegral . Country.encodeNumeric) (Country.decode t))
  (0 :: Word)
  (Text.split (=='\n') txt)
