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
  show (Array _) = "Array"
  show (If _) = "If"
  show (Then _) = "Then"
  show (Else _) = "Else"
  show (While _) = "While"
  show (For _) = "For"
  show (To _) = "To"
  show (Do _) = "Do"
  show (Let _) = "Let"
  show (In _) = "In"
  show (End _) = "End"
  show (Of _) = "Of"
  show (Break _) = "Break"
  show (Nil _) = "Nil"
  show (Function _) = "Function"
  show (Var _) = "Var"
  show (Type _) = "Type"
  show (Import _) = "Import"
  show (Primitive _) = "Primitive"
  show (Class _) = "Class"
  show (Extends _) = "Extends"
  show (Method _) = "Method"
  show (New _) = "New"
  show (Comma _) = "Comma"
  show (Colon _) = "Colon"
  show (Semicolon _) = "Semicolon"
  show (LParen _) = "LParen"
  show (RParen _) = "RParen"
  show (LBracket _) = "LBracket"
  show (RBracket _) = "RBracket"
  show (LBrace _) = "LBrace"
  show (RBrace _) = "RBrace"
  show (Dot _) = "Dot"
  show (Plus _) = "Plus"
  show (Minus _) = "Minus"
  show (Star _) = "Star"
  show (Slash _) = "Slash"
  show (EqualTo _) = "EqualTo"
  show (NotEqualTo _) = "NotEqualTo"
  show (LessThan _) = "LessThan"
  show (LessThanOrEqualTo _) = "LessThanOrEqualTo"
  show (GreaterThan _) = "GreaterThan"
  show (GreaterThanOrEqualTo _) = "GreaterThanOrEqualTo"
  show (And _) = "And"
  show (Or _) = "Or"
  show (Assign _) = "Assign"
  show (StringLiteral t _) = "StringLiteral<" ++ unpack t ++ ">"
  show (IntLiteral i _) = "IntLiteral<" ++ show i ++ ">"
  show (Identifier t _) = "Identifier<" ++ unpack t ++ ">"
  show EOF = "EOF"
  show (CommentBegin _) = "CommentBegin"
  show (CommentEnd _) = "CommentEnd"
