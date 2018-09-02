import Country (Country)
import Data.Proxy (Proxy(..))
import Test.Tasty (defaultMain,testGroup,TestTree)

import qualified Test.QuickCheck.Classes as QCC
import qualified Test.QuickCheck.Classes.IsList as QCCL
import qualified Test.Tasty.QuickCheck as TQC
import qualified Test.QuickCheck as QC

main :: IO ()
main = defaultMain $ testGroup "Country"
  [ lawsToTest (QCC.eqLaws (Proxy :: Proxy Country))
  , lawsToTest (QCC.ordLaws (Proxy :: Proxy Country))
  , lawsToTest (QCC.enumLaws (Proxy :: Proxy Country))
  ]

lawsToTest :: QCC.Laws -> TestTree
lawsToTest (QCC.Laws name pairs) = testGroup name (map (uncurry TQC.testProperty) pairs)

instance QC.Arbitrary Country where
  arbitrary = QC.arbitraryBoundedEnum


