module Tiger.Parser.Tokens where

import Data.Text (Text)
import Tiger.Util.SourcePos (SourceRegion)

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
    StringLiteral !SourceRegion !Text
  | IntLiteral !SourceRegion !Int
  | -- Identifiers
    Identifier !SourceRegion !Text
  deriving (Eq, Ord, Show)