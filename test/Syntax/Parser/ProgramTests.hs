{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Syntax.Parser.ProgramTests where

import Control.Monad (forM)

import Data.String.Interpolate (i)

import Test.Hspec

import Tiger.Syntax.AST
import Tiger.Syntax.Parser (parseExpression)
import Tiger.Util.SourcePos (uninitializedSourceRegion)

import Syntax.Parser.Util

testProgram1 :: String
testProgram1 =
  [i|
let
  type vec2 = { x : int, y : int }
  type vec2Array = array of vec2
  type intArray = array of int
  var a := vec2array [ 10 ] of vec2{ x = 0, y = 0 }
  var m := intArray [ 10 ] of 0
  var i := 0
in 
  while i < 10 do (
    a[i].x := i;
    a[i].y := i * i;
    i := i + 1
  );
  for j := 0 to 9 do (
    m[j] := modSq(a[j].x, a[j].y);
    print(m[j])
  );
  m
end
|]

expectedParseProgram1 :: Program
expectedParseProgram1 =
  ExpressionProgram
    ( LetExpression
        [ TypeDecl
            (Identifier "vec2" __)
            ( RecordType
                [ TypedField (Identifier "x" __) (Identifier "int" __) __
                , TypedField (Identifier "y" __) (Identifier "int" __) __
                ]
                __
            )
            __
        , TypeDecl
            (Identifier "vec2Array" __)
            (ArrayType (Identifier "vec2" __) __)
            __
        , TypeDecl
            (Identifier "intArray" __)
            (ArrayType (Identifier "int" __) __)
            __
        , VarDecl
            (Identifier "a" __)
            Nothing
            ( ArrayCreationExpression
                (Identifier "vec2array" __)
                (IntExpression 10 __)
                ( RecordCreationExpression
                    (Identifier "vec2" __)
                    [ (Identifier "x" __, IntExpression 0 __)
                    , (Identifier "y" __, IntExpression 0 __)
                    ]
                    __
                )
                __
            )
            __
        , VarDecl
            (Identifier "m" __)
            Nothing
            ( ArrayCreationExpression
                (Identifier "intArray" __)
                (IntExpression 10 __)
                (IntExpression 0 __)
                __
            )
            __
        , VarDecl (Identifier "i" __) Nothing (IntExpression 0 __) __
        ]
        [ WhileExpression
            ( OpExpression
                (LtOp __)
                (mkIdLValueExp "i")
                (IntExpression 10 __)
                __
            )
            ( SeqExpression
                [ AssignmentExpression
                    ( mkRecElOfArrLValue "a" "i" "x"
                    )
                    (mkIdLValueExp "i")
                    __
                , AssignmentExpression
                    ( mkRecElOfArrLValue "a" "i" "y"
                    )
                    ( OpExpression
                        (MulOp __)
                        (mkIdLValueExp "i")
                        (mkIdLValueExp "i")
                        __
                    )
                    __
                , AssignmentExpression
                    (IdLValue (Identifier "i" __) __)
                    ( OpExpression
                        (AddOp __)
                        (mkIdLValueExp "i")
                        (IntExpression 1 __)
                        __
                    )
                    __
                ]
                __
            )
            __
        , ForExpression
            (Identifier "j" __)
            (IntExpression 0 __)
            (IntExpression 9 __)
            ( SeqExpression
                [ AssignmentExpression
                    ( mkArrLValue "m" "j"
                    )
                    ( CallExpression
                        (Identifier "modSq" __)
                        [ mkRecElOfArrLValueExp "a" "j" "x"
                        , mkRecElOfArrLValueExp "a" "j" "y"
                        ]
                        __
                    )
                    __
                , CallExpression (Identifier "print" __) [mkArrLValueExp "m" "j"] __
                ]
                __
            )
            __
        , (mkIdLValueExp "m")
        ]
        __
    )
    __

testPrograms :: [(String, String, Program)]
testPrograms =
  [ ("program1", testProgram1, expectedParseProgram1)
  ]

{-
testProgram1 :: Spec
testProgram1 = do
  testProgram
    "program1"
    program1
    ( LetExpression
        [ TypeDecl
            (Identifier "vec2" __)
            ( RecordType
                [ TypedField (Identifier "x" __) (TypeAlias (Identifier "int" __) __) __
                , TypedField (Identifier "y" __) (TypeAlias (Identifier "int" __) __) __
                ]
                __
            )
            __
        , TypeDecl
            Identifier "vec2Array" __)
            (ArrayType (TypeAlias (Identifier "vec2" __) __) __)
            __
        , TypeDecl
            (Identifier "intArray" __)
            (ArrayType (TypeAlias (Identifier "int" __) __) __)
            __
        , VarDecl
            (Identifier "a" __)
            (Just (Identifier "vec2Array" __))
            ( ArrayCreationExpression
                (Identifier "vec2" __)
                (IntLiteral 10 __)
                ( RecordCreation
                    (Identifier "vec2" __)
                    [ (Identifier "x" __, IntLiteral 0 __)
                    , (Identifier "y" __, IntLiteral 0 __)
                    ]
                    __
                )
                __
            )
            __
        , VarDecl
            (Identifier "m" __)
            (Just (Identifier "intArray" __))
            ( ArrayCreation
                (Identifier "int" __)
                (IntLiteral 10 __)
                (IntLiteral 0 __)
                __
            )
            __
        , VarDecl (Identifier "i" __) (Just (Identifier "int" __)) (IntLiteral 0 __) __
        ]

        ( SeqExpression
            [ WhileExpression
                ( BinaryExpression
                    LessThan
                    (IdentifierExpression (Identifier "i" __) __)
                    (IntLiteral 10 __)
                    __
                )
                ( SeqExpression
                    [ AssignmentExpression
                        ( LValueExpression
                            ( RecordFieldLValue
                                ( ArrayLValue
                                    (IdentifierLValue (Identifier "a" __) __)
                                    (IdentifierExpression (Identifier "i" __) __)
                                    __
                                )
                                (Identifier "x" __)
                                __
                            )
                        )
                        (IdentifierExpression (Identifier "i" __) __)
                        __
                    , AssignmentExpression
                        ( LValueExpression
                            ( RecordFieldLValue
                                ( ArrayLValue
                                    (IdentifierLValue (Identifier "a" __) __)
                                    (IdentifierExpression (Identifier "i" __) __)
                                    __
                                )
                                (Identifier "i" __)
                                __
                            )
                        )
                        ( OpExpression
                            Multiply
                            (IdentifierExpression (Identifier "i" __) __)
                            (IdentifierExpression (Identifier "i" __) __)
                            __
                        )
                        __
                    ]
                    __
                )
            , LValueExpression (IdLValue (Identifier "i" __) __)
            ]
            __
        )
        __
      )
-}

parserProgramTestsSpec :: Spec
parserProgramTestsSpec = describe "Testing program parsing" $ do
  mapM_ (\(n, p, e) -> testProgram n p e) testPrograms