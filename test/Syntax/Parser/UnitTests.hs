module Syntax.Parser.UnitTests where

import Test.Hspec

import Tiger.Syntax.AST
import Tiger.Syntax.Parser (parseExpression)
import Tiger.Util.SourcePos (uninitializedSourceRegion)

parseExpression' :: String -> String
parseExpression' s = case parseExpression s of
  Left err -> "Error: " ++ show err
  Right exp -> show exp

test :: String -> String -> Spec
test input expected = it ("Parsing '" ++ input ++ "'") $ do
  parseExpression' input `shouldBe` expected

__ = uninitializedSourceRegion

testExp :: String -> Expression -> Spec
testExp input expected = it ("Parsing '" ++ input ++ "'") $ do
  parseExpression input `shouldBe` Right expected

namedTest :: String -> String -> String -> Spec
namedTest name input expected = it name $ do
  parseExpression' input `shouldBe` expected

parseLiterals :: Spec
parseLiterals = describe "Parse literals" $ do
  namedTest "Parsing nil" "nil" "nil"
  namedTest "Parsing a single integer" "1" "1"
  namedTest "Parsing a single string" "\"abc\"" "\"abc\""
  namedTest "Parsing an identifier" "abc" "abc"

arithmExprParserTestsSpec :: Spec
arithmExprParserTestsSpec = describe "Testing arithmetic expressions parsing" $ do
  describe "Testing operator parsing" $ do
    testExp "a + b" (OpExpression (AddOp __) (LValueExpression (IdLValue (Identifier "a" __) __) __) (LValueExpression (IdLValue (Identifier "b" __) __) __) __)
    testExp "a - b" (OpExpression (SubOp __) (LValueExpression (IdLValue (Identifier "a" __) __) __) (LValueExpression (IdLValue (Identifier "b" __) __) __) __)
    testExp "a * b" (OpExpression (MulOp __) (LValueExpression (IdLValue (Identifier "a" __) __) __) (LValueExpression (IdLValue (Identifier "b" __) __) __) __)
    testExp "a / b" (OpExpression (DivOp __) (LValueExpression (IdLValue (Identifier "a" __) __) __) (LValueExpression (IdLValue (Identifier "b" __) __) __) __)
    testExp "a = b" (OpExpression (EqOp __) (LValueExpression (IdLValue (Identifier "a" __) __) __) (LValueExpression (IdLValue (Identifier "b" __) __) __) __)
    testExp "a <> b" (OpExpression (NeqOp __) (LValueExpression (IdLValue (Identifier "a" __) __) __) (LValueExpression (IdLValue (Identifier "b" __) __) __) __)
    testExp "a < b" (OpExpression (LtOp __) (LValueExpression (IdLValue (Identifier "a" __) __) __) (LValueExpression (IdLValue (Identifier "b" __) __) __) __)
    testExp "a > b" (OpExpression (GtOp __) (LValueExpression (IdLValue (Identifier "a" __) __) __) (LValueExpression (IdLValue (Identifier "b" __) __) __) __)
    testExp "a <= b" (OpExpression (LeOp __) (LValueExpression (IdLValue (Identifier "a" __) __) __) (LValueExpression (IdLValue (Identifier "b" __) __) __) __)
    testExp "a >= b" (OpExpression (GeOp __) (LValueExpression (IdLValue (Identifier "a" __) __) __) (LValueExpression (IdLValue (Identifier "b" __) __) __) __)
    testExp "a & b" (OpExpression (AndOp __) (LValueExpression (IdLValue (Identifier "a" __) __) __) (LValueExpression (IdLValue (Identifier "b" __) __) __) __)
    testExp "a | b" (OpExpression (OrOp __) (LValueExpression (IdLValue (Identifier "a" __) __) __) (LValueExpression (IdLValue (Identifier "b" __) __) __) __)

  describe "Testing operator precedence" $ do
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

testIf :: Spec
testIf = do
  testExp
    "if a <= b then (print(\"true\"); a := 1) else (print(\"false\"); b := 2)"
    ( IfExpression
        ( OpExpression
            (LeOp __)
            (LValueExpression (IdLValue (Identifier "a" __) __) __)
            (LValueExpression (IdLValue (Identifier "b" __) __) __)
            __
        )
        ( SeqExpression
            [ CallExpression
                (Identifier "print" __)
                [StringExpression "true" __]
                __
            , AssignmentExpression
                (IdLValue (Identifier "a" __) __)
                (IntExpression 1 __)
                __
            ]
            __
        )
        ( Just
            ( SeqExpression
                [ CallExpression
                    (Identifier "print" __)
                    [StringExpression "false" __]
                    __
                , AssignmentExpression
                    (IdLValue (Identifier "b" __) __)
                    (IntExpression 2 __)
                    __
                ]
                __
            )
        )
        __
    )
  testExp
    "if a > 10 then if a > 2 then 2 else 3"
    ( IfExpression
        ( OpExpression
            (GtOp __)
            (LValueExpression (IdLValue (Identifier "a" __) __) __)
            (IntExpression 10 __)
            __
        )
        ( IfExpression
            ( OpExpression
                (GtOp __)
                (LValueExpression (IdLValue (Identifier "a" __) __) __)
                (IntExpression 2 __)
                __
            )
            (IntExpression 2 __)
            (Just (IntExpression 3 __))
            __
        )
        Nothing
        __
    )

testWhile :: Spec
testWhile =
  testExp
    "while a < b do (print(a); a := a + 1)"
    ( WhileExpression
        ( OpExpression
            (LtOp __)
            (LValueExpression (IdLValue (Identifier "a" __) __) __)
            (LValueExpression (IdLValue (Identifier "b" __) __) __)
            __
        )
        ( SeqExpression
            [ CallExpression
                (Identifier "print" __)
                [LValueExpression (IdLValue (Identifier "a" __) __) __]
                __
            , AssignmentExpression
                (IdLValue (Identifier "a" __) __)
                ( OpExpression
                    (AddOp __)
                    (LValueExpression (IdLValue (Identifier "a" __) __) __)
                    (IntExpression 1 __)
                    __
                )
                __
            ]
            __
        )
        __
    )

testFor :: Spec
testFor =
  testExp
    "for i := 1 to 10 do (print(i); print(i * 2))"
    ( ForExpression
        (Identifier "i" __)
        (IntExpression 1 __)
        (IntExpression 10 __)
        ( SeqExpression
            [ CallExpression
                (Identifier "print" __)
                [LValueExpression (IdLValue (Identifier "i" __) __) __]
                __
            , CallExpression
                (Identifier "print" __)
                [ OpExpression
                    (MulOp __)
                    (LValueExpression (IdLValue (Identifier "i" __) __) __)
                    (IntExpression 2 __)
                    __
                ]
                __
            ]
            __
        )
        __
    )

nonArithmExprParserTestsSpec :: Spec
nonArithmExprParserTestsSpec = describe "Testing non-arithmetic expression parsing" $ do
  testExp "a[10]" (LValueExpression (ArrayLValue (IdLValue (Identifier "a" __) __) (IntExpression 10 __) __) __)
  testExp "int[10] of 123" (ArrayCreationExpression (Identifier "int" __) (IntExpression 10 __) (IntExpression 123 __) __)
  testExp "vec{x = 1, y = 2}" (RecordCreationExpression (Identifier "vec" __) [((Identifier "x" __), IntExpression 1 __), ((Identifier "y" __), IntExpression 2 __)] __)
  testExp "(a := 2 ; c := 3)" (SeqExpression [AssignmentExpression (IdLValue (Identifier "a" __) __) (IntExpression 2 __) __, AssignmentExpression (IdLValue (Identifier "c" __) __) (IntExpression 3 __) __] __)
  testExp "a[10][2]" (LValueExpression (ArrayLValue (ArrayLValue (IdLValue (Identifier "a" __) __) (IntExpression 10 __) __) (IntExpression 2 __) __) __)
  testIf
  testWhile
  testFor

parserUnitTestsSpec :: Spec
parserUnitTestsSpec = describe "Parser unit tests" $ parallel $ do
  parseLiterals
  arithmExprParserTestsSpec
  nonArithmExprParserTestsSpec