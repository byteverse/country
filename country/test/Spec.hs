import Country (Country)
import Data.Proxy (Proxy(..))
import Test.Tasty (defaultMain,testGroup,TestTree)
import Test.Tasty.QuickCheck (testProperty,(===))

import qualified Country
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
  ]

proxy :: Proxy Country
proxy = Proxy

lawsToTest :: QCC.Laws -> TestTree
lawsToTest (QCC.Laws name pairs) = testGroup name (map (uncurry TQC.testProperty) pairs)

instance QC.Arbitrary Country where
  arbitrary = QC.arbitraryBoundedEnum
