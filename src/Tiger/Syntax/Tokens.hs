module Tiger.Syntax.Tokens where

import Data.Text (Text, unpack)
import Tiger.Util.SourcePos (HasSourceRegion (..), SourceRegion)

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
  deriving (Eq, Ord)

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
