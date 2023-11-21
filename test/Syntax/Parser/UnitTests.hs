module Syntax.Parser.UnitTests where

import Test.Hspec

import Tiger.Syntax.AST

import Syntax.Parser.Util

parseLiterals :: Spec
parseLiterals = describe "Parse literals" $ do
  testExp "nil" (NilExpression __)
  testExp "1" (IntExpression 1 __)
  testExp "-1" (NegateExpression (IntExpression 1 __) __)
  testExp "\"abc\"" (StringExpression "abc" __)
  testExp "abc" (mkIdLValueExp "abc")

arithmExprParserTestsSpec :: Spec
arithmExprParserTestsSpec = describe "Testing arithmetic expressions parsing" $ do
  describe "Testing operator parsing" $ do
    testExp "a + b" (OpExpression (AddOp __) (mkIdLValueExp "a") (mkIdLValueExp "b") __)
    testExp "a - b" (OpExpression (SubOp __) (mkIdLValueExp "a") (mkIdLValueExp "b") __)
    testExp "a * b" (OpExpression (MulOp __) (mkIdLValueExp "a") (mkIdLValueExp "b") __)
    testExp "a / b" (OpExpression (DivOp __) (mkIdLValueExp "a") (mkIdLValueExp "b") __)
    testExp "a = b" (OpExpression (EqOp __) (mkIdLValueExp "a") (mkIdLValueExp "b") __)
    testExp "a <> b" (OpExpression (NeqOp __) (mkIdLValueExp "a") (mkIdLValueExp "b") __)
    testExp "a < b" (OpExpression (LtOp __) (mkIdLValueExp "a") (mkIdLValueExp "b") __)
    testExp "a > b" (OpExpression (GtOp __) (mkIdLValueExp "a") (mkIdLValueExp "b") __)
    testExp "a <= b" (OpExpression (LeOp __) (mkIdLValueExp "a") (mkIdLValueExp "b") __)
    testExp "a >= b" (OpExpression (GeOp __) (mkIdLValueExp "a") (mkIdLValueExp "b") __)
    testExp "a & b" (OpExpression (AndOp __) (mkIdLValueExp "a") (mkIdLValueExp "b") __)
    testExp "a | b" (OpExpression (OrOp __) (mkIdLValueExp "a") (mkIdLValueExp "b") __)

  describe "Testing operator precedence" $ do
    testExp "a + b * c" (OpExpression (AddOp __) (mkIdLValueExp "a") (OpExpression (MulOp __) (mkIdLValueExp "b") (mkIdLValueExp "c") __) __)
    testExp "a * (b + c)" (OpExpression (MulOp __) (mkIdLValueExp "a") (OpExpression (AddOp __) (mkIdLValueExp "b") (mkIdLValueExp "c") __) __)
    testExp "a + b / c" (OpExpression (AddOp __) (mkIdLValueExp "a") (OpExpression (DivOp __) (mkIdLValueExp "b") (mkIdLValueExp "c") __) __)
    testExp "a * b + c * d" (OpExpression (AddOp __) (OpExpression (MulOp __) (mkIdLValueExp "a") (mkIdLValueExp "b") __) (OpExpression (MulOp __) (mkIdLValueExp "c") (mkIdLValueExp "d") __) __)
    testExp "a - b / (c * d) + e" (OpExpression (AddOp __) (OpExpression (SubOp __) (mkIdLValueExp "a") (OpExpression (DivOp __) (mkIdLValueExp "b") (OpExpression (MulOp __) (mkIdLValueExp "c") (mkIdLValueExp "d") __) __) __) (LValueExpression (IdLValue (Identifier "e" __) __) __) __)
    testExp "a + sin(b)" (OpExpression (AddOp __) (mkIdLValueExp "a") (CallExpression (Identifier "sin" __) [mkIdLValueExp "b"] __) __)
    testExp "c - -b" (OpExpression (SubOp __) (mkIdLValueExp "c") (NegateExpression (mkIdLValueExp "b") __) __)
    testExp "a + b < c * d" (OpExpression (LtOp __) (OpExpression (AddOp __) (mkIdLValueExp "a") (mkIdLValueExp "b") __) (OpExpression (MulOp __) (mkIdLValueExp "c") (mkIdLValueExp "d") __) __)
    testExp "a < b & c < d" (OpExpression (AndOp __) (OpExpression (LtOp __) (mkIdLValueExp "a") (mkIdLValueExp "b") __) (OpExpression (LtOp __) (mkIdLValueExp "c") (mkIdLValueExp "d") __) __)

testIf :: Spec
testIf = do
  testExp
    "if a <= b then (print(\"true\"); a := 1) else (print(\"false\"); b := 2)"
    ( IfExpression
        ( OpExpression
            (LeOp __)
            (mkIdLValueExp "a")
            (mkIdLValueExp "b")
            __
        )
        ( SeqExpression
            [ CallExpression
                (Identifier "print" __)
                [StringExpression "true" __]
                __
            , AssignmentExpression
                (mkIdLValue "a")
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
                    (mkIdLValue "b")
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
            (mkIdLValueExp "a")
            (IntExpression 10 __)
            __
        )
        ( IfExpression
            ( OpExpression
                (GtOp __)
                (mkIdLValueExp "a")
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
            (mkIdLValueExp "a")
            (mkIdLValueExp "b")
            __
        )
        ( SeqExpression
            [ CallExpression
                (Identifier "print" __)
                [mkIdLValueExp "a"]
                __
            , AssignmentExpression
                (mkIdLValue "a")
                ( OpExpression
                    (AddOp __)
                    (mkIdLValueExp "a")
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
                [mkIdLValueExp "i"]
                __
            , CallExpression
                (Identifier "print" __)
                [ OpExpression
                    (MulOp __)
                    (mkIdLValueExp "i")
                    (IntExpression 2 __)
                    __
                ]
                __
            ]
            __
        )
        __
    )

testLet :: Spec
testLet =
  testExp
    "let var x := 10 in while i > 0 do (print(i); i := i - 1) end"
    ( LetExpression
        [ VarDecl
            (Identifier "x" __)
            Nothing
            (IntExpression 10 __)
            __
        ]
        [ WhileExpression
            ( OpExpression
                (GtOp __)
                (mkIdLValueExp "i")
                (IntExpression 0 __)
                __
            )
            ( SeqExpression
                [ CallExpression
                    (Identifier "print" __)
                    [mkIdLValueExp "i"]
                    __
                , AssignmentExpression
                    (IdLValue (Identifier "i" __) __)
                    ( OpExpression
                        (SubOp __)
                        (mkIdLValueExp "i")
                        (IntExpression 1 __)
                        __
                    )
                    __
                ]
                __
            )
            __
        ]
        __
    )

nonArithmExprParserTestsSpec :: Spec
nonArithmExprParserTestsSpec = describe "Testing non-arithmetic expression parsing" $ do
  testExp "a[10]" (LValueExpression (ArrayLValue (mkIdLValue "a") (IntExpression 10 __) __) __)
  testExp "int[10] of 123" (ArrayCreationExpression (Identifier "int" __) (IntExpression 10 __) (IntExpression 123 __) __)
  testExp "vec{x = 1, y = 2}" (RecordCreationExpression (Identifier "vec" __) [((Identifier "x" __), IntExpression 1 __), ((Identifier "y" __), IntExpression 2 __)] __)
  testExp "(a := 2 ; c := 3)" (SeqExpression [AssignmentExpression (mkIdLValue "a") (IntExpression 2 __) __, AssignmentExpression (IdLValue (Identifier "c" __) __) (IntExpression 3 __) __] __)
  testExp "a[10][2]" (LValueExpression (ArrayLValue (ArrayLValue (mkIdLValue "a") (IntExpression 10 __) __) (IntExpression 2 __) __) __)
  testIf
  testWhile
  testFor
  testLet

parserUnitTestsSpec :: Spec
parserUnitTestsSpec = describe "Parser unit tests" $ parallel $ do
  parseLiterals
  arithmExprParserTestsSpec
  nonArithmExprParserTestsSpec