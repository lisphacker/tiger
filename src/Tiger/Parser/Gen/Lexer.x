{
module Tiger.Parser.Gen.Lexer where

import Data.Text qualified as T
import Text.Read (readMaybe)

import Tiger.Parser.Tokens qualified as Tok
import Tiger.Util.SourcePos
}
%wrapper "monadUserState"

@stringNormalChar = [ . \n ] # [ \" \\ ]
@digit = [ 0-9 ]

tiger :-
  -- Whitespace
  [\ \t\n\r]+ ;

  -- Comments
  -- "/*" { commentLevel += 1 }
  
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
  \" @stringNormalChar* \" { mkTokenWithParam Tok.StringLiteral 
                                        (\s -> Just $ T.drop 1 $ T.dropEnd 1 $ T.pack s) }

  @digit+ { mkTokenWithParam Tok.IntLiteral 
                             (\s -> readMaybe s :: Maybe Int) }

  -- Identifiers
  [a-zA-Z] [a-zA-Z0-9_]* { mkTokenWithParam Tok.Identifier 
                                            (\s -> Just $ T.pack s) }

  "_main" { mkTokenWithParam Tok.Identifier (\s -> Just $ T.pack s) }

{

data AlexUserState = AlexUserState {
  commentLevel :: Int
}

alexEOF :: Alex Tok.Token
alexEOF = pure $ Tok.EOF

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState 0

getCommentLevel :: Alex Int
getCommentLevel = commentLevel <$> alexGetUserState

incCommentLevel :: Alex ()
incCommentLevel = do
  st <- alexGetUserState
  alexSetUserState $ st { commentLevel = commentLevel st + 1 }

decCommentLevel :: Alex ()
decCommentLevel = do
  st <- alexGetUserState
  alexSetUserState $ st { commentLevel = commentLevel st - 1 }

mkToken :: (SourceRegion -> Tok.Token) -> AlexAction Tok.Token
mkToken cons ((AlexPn off r c), _, _, _) len = do
  let span = makeSpanOfLength len (SourceLocation off r c)
  let region = SourceRegion "" span
  return $ cons region

mkTokenWithParam :: (a -> SourceRegion -> Tok.Token) -> (String -> Maybe a) -> AlexAction Tok.Token
mkTokenWithParam cons parse ((AlexPn off r c), _, _, tokenStr) len = do
  let span = makeSpanFromString tokenStr (SourceLocation off r c)
  let region = SourceRegion "" span
  case parse tokenStr of
    Nothing -> alexError $ "Invalid token: " ++ tokenStr
    Just tokenVal -> return $ cons tokenVal region

tokenScan :: String -> Either String [Tok.Token]
tokenScan input = runAlex input go
  where
    go = do
      output <- alexMonadScan
      case output of
        (Tok.EOF) -> pure []
        _ -> (output :) <$> go

}