module Syntax.Parser.UnitTests where

import Test.Hspec

import Tiger.Syntax.Parser (parseExpression)

parseExpression' :: String -> Maybe String
parseExpression' s = case parseExpression s of
  Left err -> Nothing
  Right exp -> Just $ show exp

test :: String -> String -> Spec
test input expected = it ("Parsing '" ++ input ++ "'") $ do
  parseExpression' input `shouldBe` Just expected

namedTest :: String -> String -> String -> Spec
namedTest name input expected = it name $ do
  parseExpression' input `shouldBe` Just expected

parseLiterals :: Spec
parseLiterals = describe "Parse literals" $ do
  namedTest "Parsing a single integer" "1" "1"
  namedTest "Parsing a single string" "\"abc\"" "\"abc\""
  namedTest "Parsing an identifier" "abc" "abc"

parseArithmExprSpec :: Spec
parseArithmExprSpec = describe "Parse arithmetic expressions" $ do
  test "1 + 2 * 3" "(1 + (2 * 3))"
  test "1 * 2 + 3" "((1 * 2) + 3)"
  test "1 * (2 + 3)" "(1 * (2 + 3))"

parserUnitTestsSpec :: Spec
parserUnitTestsSpec = describe "Parser unit tests" $ parallel $ do
  parseLiterals
  parseArithmExprSpec