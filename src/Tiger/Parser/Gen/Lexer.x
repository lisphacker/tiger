{
module Tiger.Parser.Gen.Lexer where

import Data.Text qualified as T

import Tiger.Parser.Tokens qualified as Tok
import Tiger.Util.SourcePos
}
%wrapper "posn"

@stringChar = . # [ \" ]

tiger :-
  -- Keywords
  "array" { mkToken Tok.Array }
  "if" { mkToken Tok.If }
  "then" { mkToken Tok.Then }
  "else" { mkToken Tok.Else }
  "while" { mkToken Tok.While }
  "for" { mkToken Tok.For }
  "to" { mkToken Tok.To }
  "do" { mkToken Tok.Do }
  "let" { mkToken Tok.Let }
  "in" { mkToken Tok.In }
  "end" { mkToken Tok.End }
  "of" { mkToken Tok.Of }
  "break" { mkToken Tok.Break }
  "nil" { mkToken Tok.Nil }
  "function" { mkToken Tok.Function }
  "var" { mkToken Tok.Var }
  "type" { mkToken Tok.Type }
  "import" { mkToken Tok.Import }
  "primitive" { mkToken Tok.Primitive }
  "class" { mkToken Tok.Class }
  "extends" { mkToken Tok.Extends }
  "method" { mkToken Tok.Method }
  "new" { mkToken Tok.New }

  -- Symbols
  "," { mkToken Tok.Comma }
  ":" { mkToken Tok.Colon }
  ";" { mkToken Tok.Semicolon }
  "(" { mkToken Tok.LParen }
  ")" { mkToken Tok.RParen }
  "[" { mkToken Tok.LBracket }
  "]" { mkToken Tok.RBracket }
  "{" { mkToken Tok.LBrace }
  "}" { mkToken Tok.RBrace }
  "." { mkToken Tok.Dot }
  "+" { mkToken Tok.Plus }
  "-" { mkToken Tok.Minus }
  "*" { mkToken Tok.Star }
  "/" { mkToken Tok.Slash }
  "=" { mkToken Tok.EqualTo }
  "<>" { mkToken Tok.NotEqualTo }
  "<" { mkToken Tok.LessThan }
  "<=" { mkToken Tok.LessThanOrEqualTo }
  ">" { mkToken Tok.GreaterThan }
  ">=" { mkToken Tok.GreaterThanOrEqualTo }
  "&" { mkToken Tok.And }
  "|" { mkToken Tok.Or }
  ":=" { mkToken Tok.Assign }

  -- Literals
  \" @stringChar* \" { mkTokenWithParam Tok.StringLiteral 
                                        (\s -> T.drop 1 $ T.dropEnd 1 $ T.pack s)
                                        (\s -> length s - 2) }


{

mkToken :: (SourceRegion -> Tok.Token) -> AlexPosn -> String -> Tok.Token
mkToken cons (AlexPn off r c) tokenStr = let l = length tokenStr
                                             span = makeSpanOfLength (length tokenStr) (SourceLocation off r c)
                                             region = SourceRegion "" span
                                         in cons region

mkTokenWithParam :: (a -> SourceRegion -> Tok.Token) -> (String -> a) -> (String -> Int) -> AlexPosn -> String -> Tok.Token
mkTokenWithParam cons parse lengthOf (AlexPn off r c) tokenStr = let span = makeSpanOfLength (lengthOf tokenStr) (SourceLocation off r c)
                                                                     region = SourceRegion "" span
                                                                 in cons (parse tokenStr) region

}