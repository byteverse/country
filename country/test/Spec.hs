{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import Continent (Continent)
import Country (Country)
import Data.Char (ord)
import Data.Maybe (fromJust)
import Data.Primitive.Ptr (indexOffPtr)
import Data.Proxy (Proxy(..))
import Data.Word (Word8)
import Test.Tasty (defaultMain,testGroup,TestTree)
import Test.Tasty.QuickCheck (testProperty,(===))

import qualified Continent
import qualified Country
import qualified Data.Text as Text
import qualified Test.QuickCheck as QC
import qualified Test.QuickCheck.Classes as QCC
import qualified Test.QuickCheck.Classes.IsList as QCCL
import qualified Test.Tasty.QuickCheck as TQC

main :: IO ()
main = defaultMain $ testGroup "Country" $
  ( map lawsToTest
    $ map ($ proxy)
    $ [ QCC.boundedEnumLaws
      , QCC.eqLaws
      , QCC.ordLaws
      , QCC.primLaws
      , QCC.showLaws
      , QCC.storableLaws
      ]
  ) ++
  ( map lawsToTest
    $ map ($ Proxy @Continent)
    $ [ -- QCC.boundedEnumLaws
       QCC.eqLaws
      , QCC.ordLaws
      -- , QCC.primLaws
      -- , QCC.showLaws
      -- , QCC.storableLaws
      ]
  ) ++
  [ testProperty "encode-decode-english"
      (\x -> Just x === Country.decode (Country.encodeEnglish x))
  , testProperty "encode-decode-alpha-2-upper"
      (\x -> Just x === Country.decode (Country.alphaTwoUpper x))
  , testProperty "encode-decode-alpha-3-upper"
      (\x -> Just x === Country.decode (Country.alphaThreeUpper x))
  , testProperty "encode-decode-alpha-2-lower"
      (\x -> Just x === Country.decode (Country.alphaTwoLower x))
  , testProperty "encode-decode-alpha-3-lower"
      (\x -> Just x === Country.decode (Country.alphaThreeLower x))
  , testProperty "encode-alpha-two-upper"
      (\x ->
        let t = Country.alphaTwoUpper x
            ptr = Country.alphaTwoUpperUtf8Ptr x
        in
        (c2w (Text.index t 0), c2w (Text.index t 1))
        ===
        (indexOffPtr ptr 0 :: Word8, indexOffPtr ptr 1 :: Word8)
      )
  , testGroup "Continent"
    [ testProperty "encode-decode-alpha-upper" $ \x ->
      Just x === Continent.decodeAlpha (Continent.alphaUpper x)
    , testProperty "encode-decode-english" $ \x ->
      Just x === Continent.decodeEnglish (Continent.encodeEnglish x)
    , testProperty "country-continent-smoke-usa" $
      Continent.continent (fromJust $ Country.decodeAlphaTwo "US") === Continent.NorthAmerica
    , testProperty "country-continent-smoke-china" $
      Continent.continent (fromJust $ Country.decodeAlphaTwo "CN") === Continent.Asia
    ]
  ]

c2w :: Char -> Word8
c2w = fromIntegral . ord

proxy :: Proxy Country
proxy = Proxy

lawsToTest :: QCC.Laws -> TestTree
lawsToTest (QCC.Laws name pairs) = testGroup name (map (uncurry TQC.testProperty) pairs)

instance QC.Arbitrary Country where
  arbitrary = QC.arbitraryBoundedEnum

instance QC.Arbitrary Continent where
  arbitrary = QC.arbitraryBoundedEnum
