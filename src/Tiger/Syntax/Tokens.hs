module Tiger.Syntax.Tokens where

import Data.Text (Text, unpack)
import Tiger.Util.SourcePos (SourceSpan, Spanned (..))

data Token
  = -- keywords
    Array !SourceSpan
  | If !SourceSpan
  | Then !SourceSpan
  | Else !SourceSpan
  | While !SourceSpan
  | For !SourceSpan
  | To !SourceSpan
  | Do !SourceSpan
  | Let !SourceSpan
  | In !SourceSpan
  | End !SourceSpan
  | Of !SourceSpan
  | Break !SourceSpan
  | Nil !SourceSpan
  | Function !SourceSpan
  | Var !SourceSpan
  | Type !SourceSpan
  | Import !SourceSpan
  | Class !SourceSpan
  | Extends !SourceSpan
  | Method !SourceSpan
  | New !SourceSpan
  | -- Symbols
    Comma !SourceSpan
  | Colon !SourceSpan
  | Semicolon !SourceSpan
  | LParen !SourceSpan
  | RParen !SourceSpan
  | LBracket !SourceSpan
  | RBracket !SourceSpan
  | LBrace !SourceSpan
  | RBrace !SourceSpan
  | Dot !SourceSpan
  | Plus !SourceSpan
  | Minus !SourceSpan
  | Star !SourceSpan
  | Slash !SourceSpan
  | EqualTo !SourceSpan
  | NotEqualTo !SourceSpan
  | LessThan !SourceSpan
  | LessThanOrEqualTo !SourceSpan
  | GreaterThan !SourceSpan
  | GreaterThanOrEqualTo !SourceSpan
  | And !SourceSpan
  | Or !SourceSpan
  | Assign !SourceSpan
  | -- Literals
    StringLiteral !Text !SourceSpan
  | IntLiteral !Int !SourceSpan
  | -- Identifiers
    Identifier !Text !SourceSpan
  | -- EOF
    EOF
  | -- Comments
    CommentBegin !SourceSpan
  | CommentEnd !SourceSpan
  deriving (Ord)

instance Eq Token where
  (Array _) == (Array _) = True
  (If _) == (If _) = True
  (Then _) == (Then _) = True
  (Else _) == (Else _) = True
  (While _) == (While _) = True
  (For _) == (For _) = True
  (To _) == (To _) = True
  (Do _) == (Do _) = True
  (Let _) == (Let _) = True
  (In _) == (In _) = True
  (End _) == (End _) = True
  (Of _) == (Of _) = True
  (Break _) == (Break _) = True
  (Nil _) == (Nil _) = True
  (Function _) == (Function _) = True
  (Var _) == (Var _) = True
  (Type _) == (Type _) = True
  (Import _) == (Import _) = True
  (Class _) == (Class _) = True
  (Extends _) == (Extends _) = True
  (Method _) == (Method _) = True
  (New _) == (New _) = True
  (Comma _) == (Comma _) = True
  (Colon _) == (Colon _) = True
  (Semicolon _) == (Semicolon _) = True
  (LParen _) == (LParen _) = True
  (RParen _) == (RParen _) = True
  (LBracket _) == (LBracket _) = True
  (RBracket _) == (RBracket _) = True
  (LBrace _) == (LBrace _) = True
  (RBrace _) == (RBrace _) = True
  (Dot _) == (Dot _) = True
  (Plus _) == (Plus _) = True
  (Minus _) == (Minus _) = True
  (Star _) == (Star _) = True
  (Slash _) == (Slash _) = True
  (EqualTo _) == (EqualTo _) = True
  (NotEqualTo _) == (NotEqualTo _) = True
  (LessThan _) == (LessThan _) = True
  (LessThanOrEqualTo _) == (LessThanOrEqualTo _) = True
  (GreaterThan _) == (GreaterThan _) = True
  (GreaterThanOrEqualTo _) == (GreaterThanOrEqualTo _) = True
  (And _) == (And _) = True
  (Or _) == (Or _) = True
  (Assign _) == (Assign _) = True
  (StringLiteral s1 _) == (StringLiteral s2 _) = s1 == s2
  (IntLiteral i1 _) == (IntLiteral i2 _) = i1 == i2
  (Identifier s1 _) == (Identifier s2 _) = s1 == s2
  EOF == EOF = True
  (CommentBegin _) == (CommentBegin _) = True
  (CommentEnd _) == (CommentEnd _) = True
  _ == _ = False

instance Show Token where
  show (Array _) = "array"
  show (If _) = "if"
  show (Then _) = "then"
  show (Else _) = "else"
  show (While _) = "while"
  show (For _) = "for"
  show (To _) = "to"
  show (Do _) = "do"
  show (Let _) = "let"
  show (In _) = "in"
  show (End _) = "end"
  show (Of _) = "of"
  show (Break _) = "break"
  show (Nil _) = "nil"
  show (Function _) = "function"
  show (Var _) = "var"
  show (Type _) = "type"
  show (Import _) = "import"
  show (Class _) = "class"
  show (Extends _) = "extends"
  show (Method _) = "method"
  show (New _) = "new"
  show (Comma _) = ","
  show (Colon _) = ":"
  show (Semicolon _) = ";"
  show (LParen _) = "("
  show (RParen _) = ")"
  show (LBracket _) = "["
  show (RBracket _) = "]"
  show (LBrace _) = "{"
  show (RBrace _) = "}"
  show (Dot _) = "."
  show (Plus _) = "+"
  show (Minus _) = "-"
  show (Star _) = "*"
  show (Slash _) = "/"
  show (EqualTo _) = "="
  show (NotEqualTo _) = "<>"
  show (LessThan _) = "<"
  show (LessThanOrEqualTo _) = "<="
  show (GreaterThan _) = ">"
  show (GreaterThanOrEqualTo _) = ">="
  show (And _) = "&"
  show (Or _) = "|"
  show (Assign _) = ":="
  show (StringLiteral s _) = "STRING<" ++ show s ++ ">"
  show (IntLiteral i _) = "INT<" ++ show i ++ ">"
  show (Identifier s _) = "ID<" ++ unpack s ++ ">"
  show EOF = "EOF"
  show (CommentBegin _) = "/*"
  show (CommentEnd _) = "*/"

instance Spanned Token where
  getSpan (Array s) = s
  getSpan (If s) = s
  getSpan (Then s) = s
  getSpan (Else s) = s
  getSpan (While s) = s
  getSpan (For s) = s
  getSpan (To s) = s
  getSpan (Do s) = s
  getSpan (Let s) = s
  getSpan (In s) = s
  getSpan (End s) = s
  getSpan (Of s) = s
  getSpan (Break s) = s
  getSpan (Nil s) = s
  getSpan (Function s) = s
  getSpan (Var s) = s
  getSpan (Type s) = s
  getSpan (Import s) = s
  getSpan (Class s) = s
  getSpan (Extends s) = s
  getSpan (Method s) = s
  getSpan (New s) = s
  getSpan (Comma s) = s
  getSpan (Colon s) = s
  getSpan (Semicolon s) = s
  getSpan (LParen s) = s
  getSpan (RParen s) = s
  getSpan (LBracket s) = s
  getSpan (RBracket s) = s
  getSpan (LBrace s) = s
  getSpan (RBrace s) = s
  getSpan (Dot s) = s
  getSpan (Plus s) = s
  getSpan (Minus s) = s
  getSpan (Star s) = s
  getSpan (Slash s) = s
  getSpan (EqualTo s) = s
  getSpan (NotEqualTo s) = s
  getSpan (LessThan s) = s
  getSpan (LessThanOrEqualTo s) = s
  getSpan (GreaterThan s) = s
  getSpan (GreaterThanOrEqualTo s) = s
  getSpan (And s) = s
  getSpan (Or s) = s
  getSpan (Assign s) = s
  getSpan (StringLiteral _ s) = s
  getSpan (IntLiteral _ s) = s
  getSpan (Identifier _ s) = s
  getSpan EOF = error "EOF has no span"
  getSpan (CommentBegin s) = s
  getSpan (CommentEnd s) = s
