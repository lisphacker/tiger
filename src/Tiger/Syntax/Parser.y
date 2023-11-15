{
module Tiger.Syntax.Lexer where

import Tiger.Syntax.Tokens
}

%name tiger
%tokentype {Token}
%error { parseError }

%token
  array { Array _ }
  if { If _ }
  then { Then _ }
  else { Else _ }
  while { While _ }
  for { For _ }
  to { To _ }
  do { Do _ }
  let { Let _ }
  in { In _ }
  end { End _ }
  of { Of _ }
  break { Break _ }
  nil { Nil _ }
  function { Function _ }
  var { Var _ }
  type { Type _ }
  import { Import _ }
  primitive { Primitive _ }
  class { Class _ }
  extends { Extends _ }
  method { Method _ }
  new { New _ }

  ',' { Comma _ }
  ':' { Colon _ }
  ';' { Semicolon _ }
  '(' { LParen _ }
  ')' { RParen _ }
  '[' { LBracket _ }
  ']' { RBracket _ }
  '{' { LBrace _ }
  '}' { RBrace _ }
  '.' { Dot _ }
  '+' { Plus _ }
  '-' { Minus _ }
  '*' { Star _ }
  '/' { Slash _ }
  '=' { EqualTo _ }
  "<>" { NotEqualTo _ }
  '<' { LessThan _ }
  "<=" { LessThanOrEqualTo _ }
  '>' { GreaterThan _ }
  ">=" { GreaterThanOrEqualTo _ }
  '&' { And _ }
  '|' { Or _ }
  ':=' { Assign _ }
  string { StringLiteral _ _ }
  integer { IntLiteral _ _ }
  identifier { Identifier _ _ }

%%

program :: { Program }
  : exps = exp_list { Program exps }