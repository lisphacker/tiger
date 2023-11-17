module Syntax.Parser.UnitTests where

import Test.Hspec

import Tiger.Syntax.Parser (parseExpression)

parseExpression' :: String -> Maybe String
parseExpression' s = case parseExpression s of
  Left err -> Nothing
  Right exp -> Just $ show exp

parseLiterals :: Spec
parseLiterals = describe "Parse literals" $ do
  it "should parse a single integer" $ do
    parseExpression' "1" `shouldBe` Just "1"
  it "should parse a single string" $ do
    parseExpression' "\"abc\"" `shouldBe` Just "\"abc\""

parseArithmExprSpec :: Spec
parseArithmExprSpec = describe "Parse arithmetic expressions" $ do
  it "should parse an identifier" $ do
    parseExpression' "abc" `shouldBe` Just "abc"

parserUnitTestsSpec :: Spec
parserUnitTestsSpec = describe "Parser unit tests" $ parallel $ do
  parseLiterals
  parseArithmExprSpec