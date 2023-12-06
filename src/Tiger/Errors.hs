module Tiger.Errors where

import Tiger.Util.SourcePos (SourceLocation, SourceSpan)

data Error
  = LexicalError String
  | SyntaxError String SourceSpan
  | SyntaxErrorAtEndOfInput String