module Syntax.Parser.Util where

import Data.Text (Text)
import Test.Hspec

import Tiger.Syntax.AST
import Tiger.Syntax.Parser (parseExpression, parseProgram)
import Tiger.Util.SourcePos (SourceRegion, uninitializedSourceRegion)

__ :: SourceRegion
__ = uninitializedSourceRegion

testExp :: String -> Expression -> Spec
testExp input expected = it ("Parsing '" ++ input ++ "'") $ do
  parseExpression input `shouldBe` Right expected

testProgram :: String -> String -> Program -> Spec
testProgram name input expected = it ("Parsing '" ++ name ++ "'") $ do
  parseProgram input `shouldBe` Right expected

mkIdLValue :: Text -> LValue
mkIdLValue i = IdLValue (Identifier i __) __

mkIdLValueExp :: Text -> Expression
mkIdLValueExp i = LValueExpression (mkIdLValue i) __

mkArrLValue :: Text -> Text -> LValue
mkArrLValue a i = ArrayLValue (mkIdLValue a) (mkIdLValueExp i) __

mkArrLValueExp :: Text -> Text -> Expression
mkArrLValueExp a i = LValueExpression (mkArrLValue a i) __

mkRecElOfArrLValue :: Text -> Text -> Text -> LValue
mkRecElOfArrLValue a i el = RecordLValue (ArrayLValue (mkIdLValue a) (mkIdLValueExp i) __) (Identifier el __) __

mkRecElOfArrLValueExp :: Text -> Text -> Text -> Expression
mkRecElOfArrLValueExp a i el = LValueExpression (mkRecElOfArrLValue a i el) __