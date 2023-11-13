module Parser.Lexer where

import Control.Monad (forM_)
import Test.Hspec

import Tiger.Parser.Gen.Lexer
import Tiger.Parser.Tokens
import Tiger.Util.SourcePos

testNonParametricToken :: String -> String -> (SourceRegion -> Token) -> Spec
testNonParametricToken tokenType tokenStr tokenCons = it ("Testing " ++ tokenType ++ " " ++ tokenStr) $ do
  alexScanTokens tokenStr `shouldBe` [tokenCons (SourceRegion "" (Span (SourceLocation 0 1 1) (SourceLocation (length tokenStr - 1) 1 (length tokenStr))))]

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
testStringLiteral = describe "Testing string lexer" $ do
  testSimpleStringLiteral
 where
  testSimpleStringLiteral = it "Testing simple string literal" $ do
    alexScanTokens "\"12345\"" `shouldBe` [StringLiteral "12345" (SourceRegion "" (Span (SourceLocation 0 1 1) (SourceLocation 6 1 7)))]

lexerTestsSpec :: Spec
lexerTestsSpec = describe "Lexer tests" $ parallel $ do
  testKeywords
  testSymbols
  testStringLiteral