module Lexer.SingleTokenTests where

import Control.Monad (forM_)
import Prelude hiding (id)

import Data.Text qualified as T

import Test.Hspec

import Tiger.Syntax.Lexer
import Tiger.Syntax.Tokens
import Tiger.Util.SourcePos

testNonParametricToken :: String -> String -> (SourceRegion -> Token) -> Spec
testNonParametricToken tokenType tokenStr tokenCons = it ("Testing " ++ tokenType ++ " " ++ tokenStr) $ do
  tokenScan tokenStr `shouldBe` Right [tokenCons (SourceRegion "" (Span (SourceLocation 0 1 1) (SourceLocation (length tokenStr) 1 (length tokenStr + 1))))]

testKeywords :: Spec
testKeywords = describe "Testing keyword lexers" $ forM_ keywords (\(kw, tok) -> testNonParametricToken "keyword" kw tok)
 where
  keywords =
    [ ("array", Array)
    , ("if", If)
    , ("then", Then)
    , ("else", Else)
    , ("while", While)
    , ("for", For)
    , ("to", To)
    , ("do", Do)
    , ("let", Let)
    , ("in", In)
    , ("end", End)
    , ("of", Of)
    , ("break", Break)
    , ("nil", Nil)
    , ("function", Function)
    , ("var", Var)
    , ("type", Type)
    , ("import", Import)
    , ("primitive", Primitive)
    , ("class", Class)
    , ("extends", Extends)
    , ("method", Method)
    , ("new", New)
    ]

testSymbols :: Spec
testSymbols = describe "Testing symbol lexers" $ forM_ symbols (\(sym, tok) -> testNonParametricToken "symbol" sym tok)
 where
  symbols =
    [ (",", Comma)
    , (":", Colon)
    , (";", Semicolon)
    , ("(", LParen)
    , (")", RParen)
    , ("[", LBracket)
    , ("]", RBracket)
    , ("{", LBrace)
    , ("}", RBrace)
    , (".", Dot)
    , ("+", Plus)
    , ("-", Minus)
    , ("*", Star)
    , ("/", Slash)
    , ("=", EqualTo)
    , ("<>", NotEqualTo)
    , ("<", LessThan)
    , ("<=", LessThanOrEqualTo)
    , (">", GreaterThan)
    , (">=", GreaterThanOrEqualTo)
    , ("&", And)
    , ("|", Or)
    , (":=", Assign)
    ]

testStringLiteral :: Spec
testStringLiteral = describe "Testing string lexer" $ do
  testEmptyStringLiteral
  testSimpleStringLiteral
  testMultiLineStringLiteral
 where
  testEmptyStringLiteral = it "Testing empty string literal" $ do
    scanAndTestStr "\"\""
  testSimpleStringLiteral = it "Testing simple string literal" $ do
    scanAndTestStr "\"12345\""
  testMultiLineStringLiteral = it "Testing multi-line string literal" $ do
    scanAndTestStr "\"123\n45\""
  scanAndTestStr s = tokenScan s `shouldBe` Right [mkStrLit s]
  mkStrLit str = StringLiteral (T.drop 1 $ T.dropEnd 1 $ T.pack str) (SourceRegion "" (Span start end))
   where
    start = SourceLocation 0 1 1
    end = computeEnd start str
    computeEnd e [] = e
    computeEnd (SourceLocation o r c) (x : xs) = case x of
      '\n' -> computeEnd (SourceLocation (o + 1) (r + 1) 1) xs
      '\t' -> computeEnd (SourceLocation (o + 1) r (c + 8)) xs
      _ -> computeEnd (SourceLocation (o + 1) r (c + 1)) xs

testIntLiteral :: Spec
testIntLiteral = describe "Testing int lexer" $ do
  scanAndTestInt 0
  scanAndTestInt 8
  scanAndTestInt 12345
 where
  scanAndTestInt i = it ("Testing int " ++ show i) $ do
    tokenScan (show i) `shouldBe` Right [mkIntLit i]
  mkIntLit i = IntLiteral i (SourceRegion "" (Span start end))
   where
    start = SourceLocation 0 1 1
    end = SourceLocation l 1 (l + 1)
    l = length $ show i

testIdentifier :: Spec
testIdentifier = describe "Testing id lexer" $ do
  scanAndTestId "a"
  scanAndTestId "abc"
  scanAndTestId "a_bc2"
  scanAndTestId "_main"
  scanAndTestIdFailure "1"
  scanAndTestIdFailure "1ab"
  scanAndTestIdFailure "_ab1"
 where
  scanAndTestId id = it ("Testing id " ++ id) $ do
    tokenScan id `shouldBe` Right [mkId id]
  scanAndTestIdFailure id = it ("Testing id " ++ id) $ do
    tokenScan id `shouldNotBe` Right [mkId id]
  mkId id = Identifier (T.pack id) (SourceRegion "" (Span start end))
   where
    start = SourceLocation 0 1 1
    end = SourceLocation l 1 (l + 1)
    l = length id

testComments :: Spec
testComments = describe "Testing comment lexer" $ parallel $ do
  testComment "/* abc def 123 */"
  testComment "/* abc /* def */ 123 */"
  testInvalidComment "/* abc /* def 123 */"
  testInvalidComment "/* abc */ def 123 */"
 where
  testComment c = it ("Testing comment " ++ c) $ do
    tokenScan c `shouldBe` Right []
  testInvalidComment c = it ("Testing invalid comment " ++ c) $ do
    tokenScan c `shouldNotBe` Right []

singleTokenLexerTestsSpec :: Spec
singleTokenLexerTestsSpec = describe "Single token lexer tests" $ parallel $ do
  testKeywords
  testSymbols
  testStringLiteral
  testIntLiteral
  testIdentifier
  testComments