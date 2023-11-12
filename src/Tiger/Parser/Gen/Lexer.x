{
module Tiger.Parser.Gen.Lexer where

import Tiger.Parser.Tokens qualified as Tok
import Tiger.Util.SourcePos
}
%wrapper "posn"

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


{

mkToken :: (SourceRegion -> Tok.Token) -> AlexPosn -> String -> Tok.Token
mkToken cons (AlexPn off r c) tokenStr = let l = length tokenStr
                                             span = makeSpanOfLength (length tokenStr) (SourceLocation off r c)
                                             region = SourceRegion "" span
                                          in cons region

}