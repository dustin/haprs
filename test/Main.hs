import APRSTests

import Test.Framework.Runners.Options
import Test.Framework.Options (TestOptions'(..))
import Test.Framework (defaultMainWithOpts, interpretArgsOrExit)
import System.Environment (getArgs)

main :: IO ()
main = do opts <- interpretArgsOrExit =<< getArgs
          defaultMainWithOpts APRSTests.tests
            opts { ropt_hide_successes = Just True,
                   ropt_test_options = Just $ mempty { topt_maximum_generated_tests = Just 500 }}
