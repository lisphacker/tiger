-- Tasty makes it easy to test your code. It is a test framework that can
-- combine many different types of tests into one suite. See its website for
-- help: <http://documentup.com/feuerbach/tasty>.
import qualified Test.Tasty
-- Hspec is one of the providers for Tasty. It provides a nice syntax for
-- writing tests. Its website has more info: <https://hspec.github.io>.
import Test.Hspec
import Test.Tasty.Hspec
import Lib

main :: IO ()
main = do
    test <- testSpec "tiger" spec
    Test.Tasty.defaultMain test

spec :: Spec
spec = parallel $ do
    it "Sum of 1 and 1 is 2" $ do
        sum' 1 1 `shouldBe` 2
    it "safe operations (with relude)" $ do
        let h :: Int
            h = head $ [0]
        h `shouldBe` 0
        let t :: [Int]
            t = tail (0 : [1 .. 10])
        take 2 t `shouldBe` [1,2]
