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
parseArithmExprSpec = describe "Parse math expressions" $ do
  describe "Test operator parsing" $ do
    test "a + b" "(a + b)"
    test "a - b" "(a - b)"
    test "a * b" "(a * b)"
    test "a / b" "(a / b)"
    test "a = b" "(a = b)"
    test "a <> b" "(a <> b)"
    test "a < b" "(a < b)"
    test "a > b" "(a > b)"
    test "a <= b" "(a <= b)"
    test "a >= b" "(a >= b)"
    test "a & b" "(a & b)"
    test "a | b" "(a | b)"
  describe "Test operator precedence" $ do
    test "a + b * c" "(a + (b * c))"
    test "a * b + c" "((a * b) + c)"
    test "a * (b + c)" "(a * (b + c))"
    test "a + b / c" "(a + (b / c))"
    test "a * b + c * d" "((a * b) + (c * d))"
    test "a - b / (c * d) + e" "((a - (b / (c * d))) + e)"
    test "a + sin(b)" "(a + sin([b]))"
    test "a - b" "(a - b)"
    test "-a" "(-(a))"
    test "c - -b" "(c - (-(b)))"
    test "a + b < c * d" "((a + b) < (c * d))"

parserUnitTestsSpec :: Spec
parserUnitTestsSpec = describe "Parser unit tests" $ parallel $ do
  parseLiterals
  parseArithmExprSpec