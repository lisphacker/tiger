{
{-# LANGUAGE CPP #-}
module Tiger.Syntax.Parser where

import Data.Text (Text(..), pack)
import Tiger.Syntax.Lexer qualified as L
import Tiger.Syntax.Tokens qualified as Tok
import Tiger.Syntax.AST
import Tiger.Util.SourcePos (HasSourceRegion (..), SourceRegion, mergeSourceRegions, mergeHasSourceRegions, mergeHasSourceRegionsList, (<+>))
}

%name parseTigerProgram Program
%name parseTigerExpression Exp

%tokentype { Tok.Token }
%error { parseError }
%monad { Either String } { thenEither } { returnEither }

%right in
%nonassoc ':='
%nonassoc '&'
%nonassoc '|'
%nonassoc '=' "<>"
%nonassoc '<' "<=" '>' ">="
%left '+' '-'
%left '*' '/'

%token
  array     { Tok.Array _ }
  if        { Tok.If _ }
  then      { Tok.Then _ }
  else      { Tok.Else _ }
  while     { Tok.While _ }
  for       { Tok.For _ }
  to        { Tok.To _ }
  do        { Tok.Do _ }
  let       { Tok.Let _ }
  in        { Tok.In _ }
  end       { Tok.End _ }
  of        { Tok.Of _ }
  break     { Tok.Break _ }
  nil       { Tok.Nil _ }
  function  { Tok.Function _ }
  var       { Tok.Var _ }
  type      { Tok.Type _ }
  import    { Tok.Import _ }
  primitive { Tok.Primitive _ }
  class     { Tok.Class _ }
  extends   { Tok.Extends _ }
  method    { Tok.Method _ }
  new       { Tok.New _ }

  ',' { Tok.Comma _ }
  ':' { Tok.Colon _ }
  ';' { Tok.Semicolon _ }
  '(' { Tok.LParen _ }
  ')' { Tok.RParen _ }
  '[' { Tok.LBracket _ }
  ']' { Tok.RBracket _ }
  '{' { Tok.LBrace _ }
  '}' { Tok.RBrace _ }
  '.' { Tok.Dot _ }
  '+' { Tok.Plus _ }
  '-' { Tok.Minus _ }
  '*' { Tok.Star _ }
  '/' { Tok.Slash _ }
  '=' { Tok.EqualTo _ }
  "<>" { Tok.NotEqualTo _ }
  '<' { Tok.LessThan _ }
  "<=" { Tok.LessThanOrEqualTo _ }
  '>' { Tok.GreaterThan _ }
  ">=" { Tok.GreaterThanOrEqualTo _ }
  '&' { Tok.And _ }
  '|' { Tok.Or _ }
  ":=" { Tok.Assign _ }
  string { Tok.StringLiteral _ _ }
  integer { Tok.IntLiteral _ _ }
  identifier { Tok.Identifier _ _ }

%%

-- TypeId : identifier { mkIdent $1 }

TypedField : identifier ':' identifier { TypedField (mkIdent $1) (mkIdent $3) ($1 <+> $3) }

TypedFields : TypedField ',' TypedFields { $1:$3 }
            | TypedField { [$1] }

Type : identifier { TypeAlias (mkIdent $1) (sourceRegion $1) }
     | '{' TypedFields '}' { RecordType $2 ($1 <+> $3) }
     | array of identifier { ArrayType (mkIdent $3) ($1 <+> $3) }

ExpList : Exp ',' ExpList { $1:$3 }
        | Exp { [$1] }

FieldValues : identifier '=' Exp ',' FieldValues { (mkIdent $1,$3):$5 }
            | identifier '=' Exp { [(mkIdent $1,$3)] }

LValue : identifier { IdLValue (mkIdent $1) (sourceRegion $1) }
       | LValue2 { $1 }

LValue2 : identifier '.' identifier { RecordLValue (mkIdLVal $1) (mkIdent $3) ($1 <+> $3) } 
        | LValue2 '.' identifier { RecordLValue $1 (mkIdent $3) ($1 <+> $3) }
        | identifier '[' Exp ']' { ArrayLValue (mkIdLVal $1) $3  ($1 <+> $4) }
        | LValue2 '[' Exp ']' { ArrayLValue $1 $3  ($1 <+> $4) }

Exp : nil { NilExpression (sourceRegion $1) }
    | integer { IntExpression (getIntFromToken $1) (sourceRegion $1) }
    | string { StringExpression (getStrFromToken $1) (sourceRegion $1) }
    | Exp '+' Exp { OpExpression (AddOp $ sourceRegion $2) $1 $3 ($1 <+> $3) }
    | Exp '-' Exp { OpExpression (SubOp $ sourceRegion $2) $1 $3 ($1 <+> $3) }
    | Exp '*' Exp { OpExpression (MulOp $ sourceRegion $2) $1 $3 ($1 <+> $3) }
    | Exp '/' Exp { OpExpression (DivOp $ sourceRegion $2) $1 $3 ($1 <+> $3) }
    | Exp '=' Exp { OpExpression (EqOp $ sourceRegion $2) $1 $3 ($1 <+> $3) }
    | Exp "<>" Exp { OpExpression (NeqOp $ sourceRegion $2) $1 $3 ($1 <+> $3) }
    | Exp '<' Exp { OpExpression (LtOp $ sourceRegion $2) $1 $3 ($1 <+> $3) }
    | Exp "<=" Exp { OpExpression (LeOp $ sourceRegion $2) $1 $3 ($1 <+> $3) }
    | Exp '>' Exp { OpExpression (GtOp $ sourceRegion $2) $1 $3 ($1 <+> $3) }
    | Exp ">=" Exp { OpExpression (GeOp $ sourceRegion $2) $1 $3 ($1 <+> $3) }
    | Exp '&' Exp { OpExpression (AndOp $ sourceRegion $2) $1 $3 ($1 <+> $3) }
    | Exp '|' Exp { OpExpression (OrOp $ sourceRegion $2) $1 $3 ($1 <+> $3) }
    | '(' Exp ')' { $2 }
    | '-' Exp { NegateExpression $2 ($1 <+> $2) }
    | '(' Exps ')' { SeqExpression $2 ($1 <+> $3) }
    | identifier '[' Exp ']' of Exp { ArrayCreationExpression (mkIdent $1) $3 $6 ($1 <+> $6) }
    | identifier '{' FieldValues '}' { RecordCreationExpression (mkIdent $1) $3 ($1 <+> $4) }
    | LValue { LValueExpression $1 (sourceRegion $1) }
    | identifier '(' ExpList ')' { CallExpression (mkIdent $1) $3 ($1 <+> $4) }
    | LValue ":=" Exp { AssignmentExpression $1 $3 ($1 <+> $3) }
    | if Exp then Exp else Exp { IfExpression $2 $4 (Just $6) ($1 <+> $6) }
    | if Exp then Exp { IfExpression $2 $4 Nothing ($1 <+> $4) }
    | while Exp do Exp { WhileExpression $2 $4 ($1 <+> $4) }
    | for identifier ":=" Exp to Exp do Exp { ForExpression (mkIdent $2) $4 $6 $8 ($1 <+> $8) }
    | break { BreakExpression (sourceRegion $1) }
    | let Chunks in Exps end { LetExpression $2 $4 ($1 <+> $5) }

Exps : Exp ';' Exps { $1:$3 }
     | Exp ';' { [$1] }

VarDecl : var identifier ":=" Exp ';' { VarDecl (mkIdent $2) Nothing $4 ($1 <+> $5) }
             | var identifier ':' identifier ":=" Exp ';' { VarDecl (mkIdent $2) (Just (mkIdent $4)) $6 ($1 <+> $7) }

TypeDecl : type identifier '=' Type { TypeDecl (mkIdent $2) $4 ($1 <+> $4) }

FuncDecl : function identifier '(' TypedFields ')' ':' identifier '=' Exp { FuncDecl (mkIdent $2) $4 (Just (mkIdent $7)) $9 ($1 <+> $9) }
         | function identifier '(' TypedFields ')' '=' Exp { FuncDecl (mkIdent $2) $4 Nothing $7 ($1 <+> $7) }

PrimitiveDecl : primitive identifier '(' TypedFields ')' ':' identifier { PrimitiveDecl (mkIdent $2) $4 (Just (mkIdent $7)) ($1 <+> $7) }
              | primitive identifier '(' TypedFields ')' { PrimitiveDecl (mkIdent $2) $4 Nothing ($1 <+> $5) }

Chunk : TypeDecl { $1 }
      | VarDecl { $1 }
      | FuncDecl { $1 }
      | PrimitiveDecl { $1 }

Chunks : Chunk Chunks { $1:$2 }
       | Chunk { [$1] }

Program : Exp { ExpressionProgram $1 (sourceRegion $1) }
        | Chunks { Chunks $1 (mergeHasSourceRegionsList $1) }
      


{
thenEither :: Either String a -> (a -> Either String b) -> Either String b
thenEither (Left err) _ = Left err
thenEither (Right a) f = f a

returnEither :: a -> Either String a
returnEither = Right

parseError :: [Tok.Token] -> Either String a
parseError [] = Left $ "Parse error at end of file"
parseError (t:ts) = Left $ "Parse error reading '" ++ show t ++ "' at " <> (show $ sourceRegion t)


mkIdent :: Tok.Token -> Identifier
mkIdent (Tok.Identifier t r) = Identifier t r
mkIdent _ = error "mkIdent: not an identifier"

mkIdLVal :: Tok.Token -> LValue
mkIdLVal (Tok.Identifier t r) = IdLValue (Identifier t r) r
mkIdLVal _ = error "mkIdent: not an identifier"

getIntFromToken :: Tok.Token -> Int
getIntFromToken (Tok.IntLiteral i _) = i
getIntFromToken _ = error "getIntFromToken: not an integer"

getStrFromToken :: Tok.Token -> Text
getStrFromToken (Tok.StringLiteral s _) = s
getStrFromToken _ = error "getStrFromToken: not a string"

parseExpression :: String -> Either String Expression
parseExpression s = L.tokenScan s >>= parseTigerExpression

parseProgram :: String -> Either String Program
parseProgram s = L.tokenScan s >>= parseTigerProgram
}