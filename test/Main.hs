module Main where

-- Tasty makes it easy to test your code. It is a test framework that can
-- combine many different types of tests into one suite. See its website for
-- help: <http://documentup.com/feuerbach/tasty>.
import qualified Test.Tasty

-- Hspec is one of the providers for Tasty. It provides a nice syntax for
-- writing tests. Its website has more info: <https://hspec.github.io>.

import Test.Hspec
import Test.Tasty.Hspec

import Parser.Lexer (lexerTestsSpec)

main :: IO ()
main = do
  test <- testSpec "tiger" spec
  Test.Tasty.defaultMain test

spec :: Spec
spec = do
  lexerTestsSpec
