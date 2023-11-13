module Parser.Lexer where

import Control.Monad (forM_)

import Data.Text qualified as T

import Test.Hspec

import Tiger.Parser.Gen.Lexer
import Tiger.Parser.Tokens
import Tiger.Util.SourcePos

testNonParametricToken :: String -> String -> (SourceRegion -> Token) -> Spec
testNonParametricToken tokenType tokenStr tokenCons = it ("Testing " ++ tokenType ++ " " ++ tokenStr) $ do
  alexScanTokens tokenStr `shouldBe` [tokenCons (SourceRegion "" (Span (SourceLocation 0 1 1) (SourceLocation (length tokenStr) 1 (length tokenStr + 1))))]

testKeywords :: Spec
testKeywords = describe "Testing keyword lexers" $ parallel $ forM_ keywords (\(kw, token) -> testNonParametricToken "keyword" kw token)
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
testSymbols = describe "Testing symbol lexers" $ parallel $ forM_ symbols (\(sym, token) -> testNonParametricToken "symbol" sym token)
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
testStringLiteral = describe "Testing string lexer" $ parallel $ do
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
  scanAndTestStr s = alexScanTokens s `shouldBe` [mkStrLit s]
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
testIntLiteral = describe "Testing int lexer" $ parallel $ do
  scanAndTestInt 0
  scanAndTestInt 12345
 where
  scanAndTestInt i =  it ("Testing int " ++ show i) $ do
    alexScanTokens (show i) `shouldBe` [mkIntLit i]
  mkIntLit i = IntLiteral i (SourceRegion "" (Span start end))
   where
    start = SourceLocation 0 1 1
    end = SourceLocation l 1 (l + 1)
    l = length $ show i

testIdentifier :: Spec
testIdentifier = describe "Testing id lexer" $ parallel $ do
  scanAndTestId "a"
  scanAndTestId "abc"
  scanAndTestId "_main"
 where
  scanAndTestId id =  it ("Testing id " ++ id) $ do
    alexScanTokens id `shouldBe` [mkId id]
  mkId id = Identifier (T.pack id) (SourceRegion "" (Span start end))
   where
    start = SourceLocation 0 1 1
    end = SourceLocation l 1 (l + 1)
    l = length id

lexerTestsSpec :: Spec
lexerTestsSpec = describe "Lexer tests" $ parallel $ do
  testKeywords
  testSymbols
  testStringLiteral
  testIntLiteral
  testIdentifier