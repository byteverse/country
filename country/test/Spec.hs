import Country (Country)
import Data.Proxy (Proxy(..))
import Test.Tasty (defaultMain,testGroup,TestTree)

import qualified Test.QuickCheck.Classes as QCC
import qualified Test.QuickCheck.Classes.IsList as QCCL
import qualified Test.Tasty.QuickCheck as TQC
import qualified Test.QuickCheck as QC

main :: IO ()
main = defaultMain
  $ testGroup "Country"
  $ map lawsToTest
  $ map ($ proxy)
  $ [ QCC.boundedEnumLaws
    , QCC.eqLaws
    , QCC.ordLaws
    , QCC.primLaws
    , QCC.showLaws
    , QCC.storableLaws
    ]

proxy :: Proxy Country
proxy = Proxy

lawsToTest :: QCC.Laws -> TestTree
lawsToTest (QCC.Laws name pairs) = testGroup name (map (uncurry TQC.testProperty) pairs)

instance QC.Arbitrary Country where
  arbitrary = QC.arbitraryBoundedEnum
