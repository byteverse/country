{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}

import Data.Text (Text)
import Gauge.Main (bench, defaultMain, whnf)

import qualified Country
import qualified Data.List as List
import qualified Data.Text as Text

main :: IO ()
main =
  defaultMain
    [ bench "decode" (whnf decodeManyCountries ex1)
    ]

ex1 :: Text
{-# NOINLINE ex1 #-}
ex1 =
  Text.intercalate
    (Text.singleton '\n')
    (List.map Country.encodeEnglish [minBound .. maxBound])

decodeManyCountries :: Text -> Word
{-# NOINLINE decodeManyCountries #-}
decodeManyCountries txt =
  List.foldl'
    (\acc t -> acc + maybe 0 (fromIntegral . Country.encodeNumeric) (Country.decode t))
    (0 :: Word)
    (Text.split (== '\n') txt)
