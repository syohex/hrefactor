import           Control.Applicative
import           Test.Tasty
import           Test.Tasty.Hspec
import qualified TestHrefactor

-- the tests
tests :: IO TestTree
tests = 
  testGroup "hrefactor" <$>
  sequence [testSpec "Hrefactor" =<<
            TestHrefactor.tests]

main :: IO ()
main = defaultMain =<< tests
