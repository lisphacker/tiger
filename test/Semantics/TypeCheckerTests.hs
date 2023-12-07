module Semantics.TypeCheckerTests where

import Test.Hspec
import Tiger.Semantics.Environment (baseEnv)
import Tiger.Semantics.TypeCheck (typeCheckExpr)
import Tiger.Syntax.Parser (parseExpression)

testExp :: String -> String -> Spec
testExp name input = it ("Testing '" ++ name ++ "'") $ do
  case parseExpression input of
    Left err -> expectationFailure $ show err
    Right expr -> do
      case typeCheckExpr baseEnv expr of
        Left err -> expectationFailure $ show err
        Right _ -> pure ()

testNil :: Spec
testNil = testExp "nil" "nil"

typeCheckerTestsSpec :: Spec
typeCheckerTestsSpec = describe "Type cheecker unit tests" $ parallel $ do
  testNil