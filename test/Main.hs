import APRSTests
import FAPTests

import Test.Tasty

main :: IO ()
main = do
  faptests <- FAPTests.tests
  defaultMain $ testGroup "All Tests" [testGroup "FAP Tests" faptests,
                                       testGroup "APRS Tests" APRSTests.tests]
