module Syntax.Parser.Util where

import Data.Text (Text)
import Test.Hspec

import Tiger.Syntax.AST
import Tiger.Syntax.Parser (parseExpression)
import Tiger.Util.SourcePos (SourceLocation (..), SourceSpan (..))

__ :: SourceSpan
__ = SourceSpan (SourceLocation 0 0 0) (SourceLocation 0 0 0)

testExp :: String -> Expression SourceSpan -> Spec
testExp input expected = it ("Parsing '" ++ input ++ "'") $ do
  parseExpression input `shouldBe` Right expected

testProgram :: String -> String -> Expression SourceSpan -> Spec
testProgram name input expected = it ("Parsing program '" ++ name ++ "'") $ do
  parseExpression input `shouldBe` Right expected

mkIdLValue :: Text -> LValue SourceSpan
mkIdLValue i = IdLValue (Identifier i __) __

mkIdLValueExp :: Text -> Expression SourceSpan
mkIdLValueExp i = LValueExpression (mkIdLValue i) __

mkArrLValue :: Text -> Text -> LValue SourceSpan
mkArrLValue a i = ArrayLValue (mkIdLValue a) (mkIdLValueExp i) __

mkArrLValueExp :: Text -> Text -> Expression SourceSpan
mkArrLValueExp a i = LValueExpression (mkArrLValue a i) __

mkRecElOfArrLValue :: Text -> Text -> Text -> LValue SourceSpan
mkRecElOfArrLValue a i el = RecordLValue (ArrayLValue (mkIdLValue a) (mkIdLValueExp i) __) (Identifier el __) __

mkRecElOfArrLValueExp :: Text -> Text -> Text -> Expression SourceSpan
mkRecElOfArrLValueExp a i el = LValueExpression (mkRecElOfArrLValue a i el) __