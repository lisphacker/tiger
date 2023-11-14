{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Lexer.ProgramTests where

import Data.String.Interpolate (i)

import Test.Hspec

import Tiger.Parser.Lexer (tokenScan)

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
  tokenScan prog `shouldBe` Right []

programLexerTestsSpec :: Spec
programLexerTestsSpec = describe "Program lexer tests" $ parallel $ do
  testEmptyProgram
  testProgram