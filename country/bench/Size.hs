{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}

import Country (hashMapUtf16, hashMapUtf8)
import Data.Compact
import Data.Foldable

import qualified Country
import qualified Data.Bytes.HashMap.Word as Map
import qualified Data.List as List

main :: IO ()
main = do
  szUtf8 <- estimateHeapUse hashMapUtf8
  putStrLn ("UTF-8 HashMap Size:  " ++ show szUtf8)
  szUtf16 <- estimateHeapUse hashMapUtf16
  putStrLn ("UTF-16 HashMap Size: " ++ show szUtf16)
  putStrLn "UTF-8 HashMap Distribution:"
  forM_ (Map.distribution hashMapUtf8) $ \(bktSz, ct) ->
    putStrLn (show bktSz ++ "," ++ show ct)
  putStrLn ("UTF-8 HashMap Distinct Entropies: " ++ show (Map.distinctEntropies hashMapUtf8))

-- I think this rounds to the nearest 4KB.
estimateHeapUse :: a -> IO Word
estimateHeapUse a =
  foldlM
    ( \lo i -> do
        w <- compactSized (i * 2000) True a >>= compactSize
        pure (min lo w)
    )
    maxBound
    [1 .. 50]
