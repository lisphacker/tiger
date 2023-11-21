module Tiger.Syntax.Tokens where

import Data.Text (Text, unpack)
import Tiger.Util.SourcePos (HasSourceRegion (..), SourceRegion, uninitializedSourceRegion)

data Token
  = -- keywords
    Array !SourceRegion
  | If !SourceRegion
  | Then !SourceRegion
  | Else !SourceRegion
  | While !SourceRegion
  | For !SourceRegion
  | To !SourceRegion
  | Do !SourceRegion
  | Let !SourceRegion
  | In !SourceRegion
  | End !SourceRegion
  | Of !SourceRegion
  | Break !SourceRegion
  | Nil !SourceRegion
  | Function !SourceRegion
  | Var !SourceRegion
  | Type !SourceRegion
  | Import !SourceRegion
  | Primitive !SourceRegion
  | Class !SourceRegion
  | Extends !SourceRegion
  | Method !SourceRegion
  | New !SourceRegion
  | -- Symbols
    Comma !SourceRegion
  | Colon !SourceRegion
  | Semicolon !SourceRegion
  | LParen !SourceRegion
  | RParen !SourceRegion
  | LBracket !SourceRegion
  | RBracket !SourceRegion
  | LBrace !SourceRegion
  | RBrace !SourceRegion
  | Dot !SourceRegion
  | Plus !SourceRegion
  | Minus !SourceRegion
  | Star !SourceRegion
  | Slash !SourceRegion
  | EqualTo !SourceRegion
  | NotEqualTo !SourceRegion
  | LessThan !SourceRegion
  | LessThanOrEqualTo !SourceRegion
  | GreaterThan !SourceRegion
  | GreaterThanOrEqualTo !SourceRegion
  | And !SourceRegion
  | Or !SourceRegion
  | Assign !SourceRegion
  | -- Literals
    StringLiteral !Text !SourceRegion
  | IntLiteral !Int !SourceRegion
  | -- Identifiers
    Identifier !Text !SourceRegion
  | -- EOF
    EOF
  | -- Comments
    CommentBegin !SourceRegion
  | CommentEnd !SourceRegion
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
  (Primitive _) == (Primitive _) = True
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

instance HasSourceRegion Token where
  sourceRegion (Array r) = r
  sourceRegion (If r) = r
  sourceRegion (Then r) = r
  sourceRegion (Else r) = r
  sourceRegion (While r) = r
  sourceRegion (For r) = r
  sourceRegion (To r) = r
  sourceRegion (Do r) = r
  sourceRegion (Let r) = r
  sourceRegion (In r) = r
  sourceRegion (End r) = r
  sourceRegion (Of r) = r
  sourceRegion (Break r) = r
  sourceRegion (Nil r) = r
  sourceRegion (Function r) = r
  sourceRegion (Var r) = r
  sourceRegion (Type r) = r
  sourceRegion (Import r) = r
  sourceRegion (Primitive r) = r
  sourceRegion (Class r) = r
  sourceRegion (Extends r) = r
  sourceRegion (Method r) = r
  sourceRegion (New r) = r
  sourceRegion (Comma r) = r
  sourceRegion (Colon r) = r
  sourceRegion (Semicolon r) = r
  sourceRegion (LParen r) = r
  sourceRegion (RParen r) = r
  sourceRegion (LBracket r) = r
  sourceRegion (RBracket r) = r
  sourceRegion (LBrace r) = r
  sourceRegion (RBrace r) = r
  sourceRegion (Dot r) = r
  sourceRegion (Plus r) = r
  sourceRegion (Minus r) = r
  sourceRegion (Star r) = r
  sourceRegion (Slash r) = r
  sourceRegion (EqualTo r) = r
  sourceRegion (NotEqualTo r) = r
  sourceRegion (LessThan r) = r
  sourceRegion (LessThanOrEqualTo r) = r
  sourceRegion (GreaterThan r) = r
  sourceRegion (GreaterThanOrEqualTo r) = r
  sourceRegion (And r) = r
  sourceRegion (Or r) = r
  sourceRegion (Assign r) = r
  sourceRegion (StringLiteral _ r) = r
  sourceRegion (IntLiteral _ r) = r
  sourceRegion (Identifier _ r) = r
  sourceRegion EOF = uninitializedSourceRegion
  sourceRegion (CommentBegin r) = r
  sourceRegion (CommentEnd r) = r

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
  show (Primitive _) = "primitive"
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
  show (StringLiteral s _) = show s
  show (IntLiteral i _) = show i
  show (Identifier s _) = unpack s
  show EOF = "EOF"
  show (CommentBegin _) = "/*"
  show (CommentEnd _) = "*/"
