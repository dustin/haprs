import APRSTests
import FAPTests
import MicETests
import ISTests

import Test.Tasty

main :: IO ()
main = do
  faptests <- FAPTests.tests
  defaultMain $ testGroup "All Tests" [faptests,
                                       testGroup "APRS Tests" APRSTests.tests,
                                       testGroup "MicE Tests" MicETests.tests,
                                       testGroup "APRS-IS Tests" ISTests.tests]
