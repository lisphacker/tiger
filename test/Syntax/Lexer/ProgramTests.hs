{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Syntax.Lexer.ProgramTests where

import Data.String.Interpolate (i)

import Test.Hspec

import Tiger.Syntax.Lexer (tokenScan)
import Tiger.Syntax.Tokens
import Tiger.Util.SourcePos (SourceLocation (..), SourceSpan (..))

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

__ :: SourceSpan
__ = SourceSpan (SourceLocation 0 0 0) (SourceLocation 0 0 0)

testProgram :: Spec
testProgram = it "Testing program" $ do
  tokenScan prog
    `shouldBe` Right
      [ Let __
      , Var __
      , Identifier "N" __
      , Assign __
      , IntLiteral 8 __
      , Type __
      , Identifier "intArray" __
      , EqualTo __
      , Array __
      , Of __
      , Identifier "int" __
      , Var __
      , Identifier "row" __
      , Assign __
      , Identifier "intArray" __
      , LBracket __
      , Identifier "N" __
      , RBracket __
      , Of __
      , IntLiteral 0 __
      , Var __
      , Identifier "col" __
      , Assign __
      , Identifier "intArray" __
      , LBracket __
      , Identifier "N" __
      , RBracket __
      , Of __
      , IntLiteral 0 __
      , Var __
      , Identifier "diag1" __
      , Assign __
      , Identifier "intArray" __
      , LBracket __
      , Identifier "N" __
      , Plus __
      , Identifier "N" __
      , Minus __
      , IntLiteral 1 __
      , RBracket __
      , Of __
      , IntLiteral 0 __
      , Var __
      , Identifier "diag2" __
      , Assign __
      , Identifier "intArray" __
      , LBracket __
      , Identifier "N" __
      , Plus __
      , Identifier "N" __
      , Minus __
      , IntLiteral 1 __
      , RBracket __
      , Of __
      , IntLiteral 0 __
      , Function __
      , Identifier "printboard" __
      , LParen __
      , RParen __
      , EqualTo __
      , LParen __
      , For __
      , Identifier "i" __
      , Assign __
      , IntLiteral 0 __
      , To __
      , Identifier "N" __
      , Minus __
      , IntLiteral 1 __
      , Do __
      , LParen __
      , For __
      , Identifier "j" __
      , Assign __
      , IntLiteral 0 __
      , To __
      , Identifier "N" __
      , Minus __
      , IntLiteral 1 __
      , Do __
      , Identifier "print" __
      , LParen __
      , If __
      , Identifier "col" __
      , LBracket __
      , Identifier "i" __
      , RBracket __
      , EqualTo __
      , Identifier "j" __
      , Then __
      , StringLiteral " O" __
      , Else __
      , StringLiteral " ." __
      , RParen __
      , Semicolon __
      , Identifier "print" __
      , LParen __
      , StringLiteral "\n" __
      , RParen __
      , RParen __
      , Semicolon __
      , Identifier "print" __
      , LParen __
      , StringLiteral "\n" __
      , RParen __
      , RParen __
      , Function __
      , Identifier "try" __
      , LParen __
      , Identifier "c" __
      , Colon __
      , Identifier "int" __
      , RParen __
      , EqualTo __
      , LParen __
      , If __
      , Identifier "c" __
      , EqualTo __
      , Identifier "N" __
      , Then __
      , Identifier "printboard" __
      , LParen __
      , RParen __
      , Else __
      , For __
      , Identifier "r" __
      , Assign __
      , IntLiteral 0 __
      , To __
      , Identifier "N" __
      , Minus __
      , IntLiteral 1 __
      , Do __
      , If __
      , Identifier "row" __
      , LBracket __
      , Identifier "r" __
      , RBracket __
      , EqualTo __
      , IntLiteral 0 __
      , And __
      , Identifier "diag1" __
      , LBracket __
      , Identifier "r" __
      , Plus __
      , Identifier "c" __
      , RBracket __
      , EqualTo __
      , IntLiteral 0 __
      , And __
      , Identifier "diag2" __
      , LBracket __
      , Identifier "r" __
      , Plus __
      , IntLiteral 7 __
      , Minus __
      , Identifier "c" __
      , RBracket __
      , EqualTo __
      , IntLiteral 0 __
      , Then __
      , LParen __
      , Identifier "row" __
      , LBracket __
      , Identifier "r" __
      , RBracket __
      , Assign __
      , IntLiteral 1 __
      , Semicolon __
      , Identifier "diag1" __
      , LBracket __
      , Identifier "r" __
      , Plus __
      , Identifier "c" __
      , RBracket __
      , Assign __
      , IntLiteral 1 __
      , Semicolon __
      , Identifier "diag2" __
      , LBracket __
      , Identifier "r" __
      , Plus __
      , IntLiteral 7 __
      , Minus __
      , Identifier "c" __
      , RBracket __
      , Assign __
      , IntLiteral 1 __
      , Semicolon __
      , Identifier "col" __
      , LBracket __
      , Identifier "c" __
      , RBracket __
      , Assign __
      , Identifier "r" __
      , Semicolon __
      , Identifier "try" __
      , LParen __
      , Identifier "c" __
      , Plus __
      , IntLiteral 1 __
      , RParen __
      , Semicolon __
      , Identifier "row" __
      , LBracket __
      , Identifier "r" __
      , RBracket __
      , Assign __
      , IntLiteral 0 __
      , Semicolon __
      , Identifier "diag1" __
      , LBracket __
      , Identifier "r" __
      , Plus __
      , Identifier "c" __
      , RBracket __
      , Assign __
      , IntLiteral 0 __
      , Semicolon __
      , Identifier "diag2" __
      , LBracket __
      , Identifier "r" __
      , Plus __
      , IntLiteral 7 __
      , Minus __
      , Identifier "c" __
      , RBracket __
      , Assign __
      , IntLiteral 0 __
      , RParen __
      , RParen __
      , In __
      , Identifier "try" __
      , LParen __
      , IntLiteral 0 __
      , RParen __
      , End __
      ]

programLexerTestsSpec :: Spec
programLexerTestsSpec = describe "Program lexer tests" $ parallel $ do
  testEmptyProgram
  testProgram