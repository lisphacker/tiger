{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Lexer.ProgramTests where

import Data.String.Interpolate (i)

import Test.Hspec

import Tiger.Syntax.Lexer (tokenScan)

testEmptyProgram :: Spec
testEmptyProgram = it "Testing empty program" $ do
  tokenScan "" `shouldBe` Right []

-- /* A program to solve the 8-queens problem */
prog :: String
prog =
  [i|/* A program to solve the 8-queens problem */

let
    var N := 8

    type intArray = array of int

    var row := intArray [ N ] of 0
    var col := intArray [ N ] of 0
    var diag1 := intArray [N+N-1] of 0
    var diag2 := intArray [N+N-1] of 0

    function printboard() =
       (for i := 0 to N-1
	 do (for j := 0 to N-1 
	      do print(if col[i]=j then " O" else " .");
	     print("\n"));
         print("\n"))

    function try(c:int) = 
( /*  for i:= 0 to c do print("."); print("\n"); flush();*/
     if c=N
     then printboard()
     else for r := 0 to N-1
	   do if row[r]=0 & diag1[r+c]=0 & diag2[r+7-c]=0
	           then (row[r]:=1; diag1[r+c]:=1; diag2[r+7-c]:=1;
		         col[c]:=r;
	                 try(c+1);
			 row[r]:=0; diag1[r+c]:=0; diag2[r+7-c]:=0)

)
 in try(0)
end
|] ::
    String

testProgram :: Spec
testProgram = it "Testing program" $ do
  fmap show <$> tokenScan prog
    `shouldBe` Right
      [ "Let"
      , "Var"
      , "Identifier<N>"
      , "Assign"
      , "IntLiteral<8>"
      , "Type"
      , "Identifier<intArray>"
      , "EqualTo"
      , "Array"
      , "Of"
      , "Identifier<int>"
      , "Var"
      , "Identifier<row>"
      , "Assign"
      , "Identifier<intArray>"
      , "LBracket"
      , "Identifier<N>"
      , "RBracket"
      , "Of"
      , "IntLiteral<0>"
      , "Var"
      , "Identifier<col>"
      , "Assign"
      , "Identifier<intArray>"
      , "LBracket"
      , "Identifier<N>"
      , "RBracket"
      , "Of"
      , "IntLiteral<0>"
      , "Var"
      , "Identifier<diag1>"
      , "Assign"
      , "Identifier<intArray>"
      , "LBracket"
      , "Identifier<N>"
      , "Plus"
      , "Identifier<N>"
      , "Minus"
      , "IntLiteral<1>"
      , "RBracket"
      , "Of"
      , "IntLiteral<0>"
      , "Var"
      , "Identifier<diag2>"
      , "Assign"
      , "Identifier<intArray>"
      , "LBracket"
      , "Identifier<N>"
      , "Plus"
      , "Identifier<N>"
      , "Minus"
      , "IntLiteral<1>"
      , "RBracket"
      , "Of"
      , "IntLiteral<0>"
      , "Function"
      , "Identifier<printboard>"
      , "LParen"
      , "RParen"
      , "EqualTo"
      , "LParen"
      , "For"
      , "Identifier<i>"
      , "Assign"
      , "IntLiteral<0>"
      , "To"
      , "Identifier<N>"
      , "Minus"
      , "IntLiteral<1>"
      , "Do"
      , "LParen"
      , "For"
      , "Identifier<j>"
      , "Assign"
      , "IntLiteral<0>"
      , "To"
      , "Identifier<N>"
      , "Minus"
      , "IntLiteral<1>"
      , "Do"
      , "Identifier<print>"
      , "LParen"
      , "If"
      , "Identifier<col>"
      , "LBracket"
      , "Identifier<i>"
      , "RBracket"
      , "EqualTo"
      , "Identifier<j>"
      , "Then"
      , "StringLiteral< O>"
      , "Else"
      , "StringLiteral< .>"
      , "RParen"
      , "Semicolon"
      , "Identifier<print>"
      , "LParen"
      , "StringLiteral<\n>"
      , "RParen"
      , "RParen"
      , "Semicolon"
      , "Identifier<print>"
      , "LParen"
      , "StringLiteral<\n>"
      , "RParen"
      , "RParen"
      , "Function"
      , "Identifier<try>"
      , "LParen"
      , "Identifier<c>"
      , "Colon"
      , "Identifier<int>"
      , "RParen"
      , "EqualTo"
      , "LParen"
      , "If"
      , "Identifier<c>"
      , "EqualTo"
      , "Identifier<N>"
      , "Then"
      , "Identifier<printboard>"
      , "LParen"
      , "RParen"
      , "Else"
      , "For"
      , "Identifier<r>"
      , "Assign"
      , "IntLiteral<0>"
      , "To"
      , "Identifier<N>"
      , "Minus"
      , "IntLiteral<1>"
      , "Do"
      , "If"
      , "Identifier<row>"
      , "LBracket"
      , "Identifier<r>"
      , "RBracket"
      , "EqualTo"
      , "IntLiteral<0>"
      , "And"
      , "Identifier<diag1>"
      , "LBracket"
      , "Identifier<r>"
      , "Plus"
      , "Identifier<c>"
      , "RBracket"
      , "EqualTo"
      , "IntLiteral<0>"
      , "And"
      , "Identifier<diag2>"
      , "LBracket"
      , "Identifier<r>"
      , "Plus"
      , "IntLiteral<7>"
      , "Minus"
      , "Identifier<c>"
      , "RBracket"
      , "EqualTo"
      , "IntLiteral<0>"
      , "Then"
      , "LParen"
      , "Identifier<row>"
      , "LBracket"
      , "Identifier<r>"
      , "RBracket"
      , "Assign"
      , "IntLiteral<1>"
      , "Semicolon"
      , "Identifier<diag1>"
      , "LBracket"
      , "Identifier<r>"
      , "Plus"
      , "Identifier<c>"
      , "RBracket"
      , "Assign"
      , "IntLiteral<1>"
      , "Semicolon"
      , "Identifier<diag2>"
      , "LBracket"
      , "Identifier<r>"
      , "Plus"
      , "IntLiteral<7>"
      , "Minus"
      , "Identifier<c>"
      , "RBracket"
      , "Assign"
      , "IntLiteral<1>"
      , "Semicolon"
      , "Identifier<col>"
      , "LBracket"
      , "Identifier<c>"
      , "RBracket"
      , "Assign"
      , "Identifier<r>"
      , "Semicolon"
      , "Identifier<try>"
      , "LParen"
      , "Identifier<c>"
      , "Plus"
      , "IntLiteral<1>"
      , "RParen"
      , "Semicolon"
      , "Identifier<row>"
      , "LBracket"
      , "Identifier<r>"
      , "RBracket"
      , "Assign"
      , "IntLiteral<0>"
      , "Semicolon"
      , "Identifier<diag1>"
      , "LBracket"
      , "Identifier<r>"
      , "Plus"
      , "Identifier<c>"
      , "RBracket"
      , "Assign"
      , "IntLiteral<0>"
      , "Semicolon"
      , "Identifier<diag2>"
      , "LBracket"
      , "Identifier<r>"
      , "Plus"
      , "IntLiteral<7>"
      , "Minus"
      , "Identifier<c>"
      , "RBracket"
      , "Assign"
      , "IntLiteral<0>"
      , "RParen"
      , "RParen"
      , "In"
      , "Identifier<try>"
      , "LParen"
      , "IntLiteral<0>"
      , "RParen"
      , "End"
      ]

programLexerTestsSpec :: Spec
programLexerTestsSpec = describe "Program lexer tests" $ parallel $ do
  testEmptyProgram
  testProgram