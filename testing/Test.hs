import qualified TestStarter

import           Control.Applicative
import           Test.Tasty
import           Test.Tasty.Hspec

-- the tests
tests :: IO TestTree
tests = testGroup "starter" <$> sequence
  [ testSpec "Starter" =<< TestStarter.tests
  ]

main :: IO ()
main = defaultMain =<< tests
