module Tiger.Syntax.AST where

import Data.List (intercalate)
import Data.Text (Text, unpack)
import Tiger.Util.SourcePos (HasSourceRegion (..), SourceRegion)

class (HasSourceRegion a) => ASTNode a

data Identifier = Identifier Text !SourceRegion

instance Eq Identifier where
  (Identifier n1 _) == (Identifier n2 _) = n1 == n2

instance Show Identifier where
  show (Identifier s _) = unpack s

instance HasSourceRegion Identifier where
  sourceRegion (Identifier _ r) = r

type TypeIdentifier = Identifier

data TypedField = TypedField Identifier TypeIdentifier !SourceRegion
  deriving (Show)

instance Eq TypedField where
  (TypedField i1 ti1 _) == (TypedField i2 ti2 _) = i1 == i2 && ti1 == ti2

instance HasSourceRegion TypedField where
  sourceRegion (TypedField _ _ r) = r

data Type
  = TypeAlias TypeIdentifier !SourceRegion
  | RecordType [TypedField] !SourceRegion
  | ArrayType TypeIdentifier !SourceRegion

instance Eq Type where
  (TypeAlias ti1 _) == (TypeAlias ti2 _) = ti1 == ti2
  (RecordType fs1 _) == (RecordType fs2 _) = fs1 == fs2
  (ArrayType ti1 _) == (ArrayType ti2 _) = ti1 == ti2

instance Show Type where
  show (TypeAlias ti _) = show ti
  show (RecordType fs _) = "{" ++ fieldsStr ++ "}"
   where
    fieldsStr = intercalate "," $ map (\(TypedField i t _) -> show i ++ " : " ++ show t) fs
  show (ArrayType ti _) = "array of " ++ show ti

instance HasSourceRegion Type where
  sourceRegion (TypeAlias _ r) = r
  sourceRegion (RecordType _ r) = r
  sourceRegion (ArrayType _ r) = r

data LValue
  = IdLValue Identifier !SourceRegion
  | RecordLValue LValue Identifier !SourceRegion
  | ArrayLValue LValue Expression !SourceRegion

instance Eq LValue where
  (IdLValue i1 _) == (IdLValue i2 _) = i1 == i2
  (RecordLValue l1 i1 _) == (RecordLValue l2 i2 _) = l1 == l2 && i1 == i2
  (ArrayLValue l1 e1 _) == (ArrayLValue l2 e2 _) = l1 == l2 && e1 == e2

instance Show LValue where
  show (IdLValue i _) = show i
  show (RecordLValue l i _) = show l ++ "." ++ show i
  show (ArrayLValue l e _) = show l ++ "[" ++ show e ++ "]"

instance HasSourceRegion LValue where
  sourceRegion (IdLValue _ r) = r
  sourceRegion (RecordLValue _ _ r) = r
  sourceRegion (ArrayLValue _ _ r) = r

data BinaryOperator
  = AddOp !SourceRegion
  | SubOp !SourceRegion
  | MulOp !SourceRegion
  | DivOp !SourceRegion
  | EqOp !SourceRegion
  | NeqOp !SourceRegion
  | LtOp !SourceRegion
  | LeOp !SourceRegion
  | GtOp !SourceRegion
  | GeOp !SourceRegion
  | AndOp !SourceRegion
  | OrOp !SourceRegion

instance Eq BinaryOperator where
  (AddOp _) == (AddOp _) = True
  (SubOp _) == (SubOp _) = True
  (MulOp _) == (MulOp _) = True
  (DivOp _) == (DivOp _) = True
  (EqOp _) == (EqOp _) = True
  (NeqOp _) == (NeqOp _) = True
  (LtOp _) == (LtOp _) = True
  (LeOp _) == (LeOp _) = True
  (GtOp _) == (GtOp _) = True
  (GeOp _) == (GeOp _) = True
  (AndOp _) == (AndOp _) = True
  (OrOp _) == (OrOp _) = True
  _ == _ = False

instance Show BinaryOperator where
  show (AddOp _) = "+"
  show (SubOp _) = "-"
  show (MulOp _) = "*"
  show (DivOp _) = "/"
  show (EqOp _) = "="
  show (NeqOp _) = "<>"
  show (LtOp _) = "<"
  show (LeOp _) = "<="
  show (GtOp _) = ">"
  show (GeOp _) = ">="
  show (AndOp _) = "&"
  show (OrOp _) = "|"

instance HasSourceRegion BinaryOperator where
  sourceRegion (AddOp r) = r
  sourceRegion (SubOp r) = r
  sourceRegion (MulOp r) = r
  sourceRegion (DivOp r) = r
  sourceRegion (EqOp r) = r
  sourceRegion (NeqOp r) = r
  sourceRegion (LtOp r) = r
  sourceRegion (LeOp r) = r
  sourceRegion (GtOp r) = r
  sourceRegion (GeOp r) = r
  sourceRegion (AndOp r) = r
  sourceRegion (OrOp r) = r

data Expression
  = NilExpression !SourceRegion
  | IntExpression Int !SourceRegion
  | StringExpression Text !SourceRegion
  | ArrayCreationExpression TypeIdentifier Expression Expression !SourceRegion
  | RecordCreationExpression TypeIdentifier [(Identifier, Expression)] !SourceRegion
  | LValueExpression LValue !SourceRegion
  | CallExpression Identifier [Expression] !SourceRegion
  | NegateExpression Expression !SourceRegion
  | OpExpression BinaryOperator Expression Expression !SourceRegion
  | SeqExpression [Expression] !SourceRegion
  | AssignmentExpression LValue Expression !SourceRegion
  | IfExpression Expression Expression (Maybe Expression) !SourceRegion
  | WhileExpression Expression Expression !SourceRegion
  | ForExpression Identifier Expression Expression Expression !SourceRegion
  | BreakExpression !SourceRegion
  | LetExpression [Chunk] [Expression] !SourceRegion

instance Eq Expression where
  (NilExpression _) == (NilExpression _) = True
  (IntExpression i1 _) == (IntExpression i2 _) = i1 == i2
  (StringExpression s1 _) == (StringExpression s2 _) = s1 == s2
  (ArrayCreationExpression t1 s1 v1 _) == (ArrayCreationExpression t2 s2 v2 _) = t1 == t2 && s1 == s2 && v1 == v2
  (RecordCreationExpression t1 fs1 _) == (RecordCreationExpression t2 fs2 _) = t1 == t2 && fs1 == fs2
  (LValueExpression l1 _) == (LValueExpression l2 _) = l1 == l2
  (CallExpression f1 args1 _) == (CallExpression f2 args2 _) = f1 == f2 && args1 == args2
  (NegateExpression e1 _) == (NegateExpression e2 _) = e1 == e2
  (OpExpression op1 e11 e12 _) == (OpExpression op2 e21 e22 _) = op1 == op2 && e11 == e21 && e12 == e22
  (SeqExpression es1 _) == (SeqExpression es2 _) = es1 == es2
  (AssignmentExpression l1 e1 _) == (AssignmentExpression l2 e2 _) = l1 == l2 && e1 == e2
  (IfExpression c1 t1 e1 _) == (IfExpression c2 t2 e2 _) = c1 == c2 && t1 == t2 && e1 == e2
  (WhileExpression c1 b1 _) == (WhileExpression c2 b2 _) = c1 == c2 && b1 == b2
  (ForExpression i1 s1 e1 b1 _) == (ForExpression i2 s2 e2 b2 _) = i1 == i2 && s1 == s2 && e1 == e2 && b1 == b2
  (BreakExpression _) == (BreakExpression _) = True
  (LetExpression cs1 es1 _) == (LetExpression cs2 es2 _) = cs1 == cs2 && es1 == es2
  _ == _ = False

instance Show Expression where
  show (NilExpression _) = "nil"
  show (IntExpression i _) = show i
  show (StringExpression s _) = show s
  show (ArrayCreationExpression t s v _) = show t ++ "[" ++ show s ++ "] of " ++ show v
  show (RecordCreationExpression t fs _) = show t ++ "{" ++ fieldsStr ++ "}"
   where
    fieldsStr = intercalate "," $ map (\(i, e) -> show i ++ " = " ++ show e) fs
  show (LValueExpression l _) = show l
  show (CallExpression f args _) = show f ++ "(" ++ show args ++ ")"
  show (NegateExpression e _) = "(-(" ++ show e ++ "))"
  show (OpExpression op e1 e2 _) = "(" ++ show e1 ++ " " ++ show op ++ " " ++ show e2 ++ ")"
  show (SeqExpression es _) = "(" ++ intercalate ";" (map show es) ++ ")"
  show (AssignmentExpression l e _) = "(" ++ show l ++ " := " ++ show e ++ ")"
  show (IfExpression c t Nothing _) = "if " ++ show c ++ " then " ++ show t
  show (IfExpression c t (Just e) _) = "if " ++ show c ++ " then " ++ show t ++ " else " ++ show e
  show (WhileExpression c b _) = "while " ++ show c ++ " do " ++ show b
  show (ForExpression i s e b _) = "for " ++ show i ++ " := " ++ show s ++ " to " ++ show e ++ " do " ++ show b
  show (BreakExpression _) = "break"
  show (LetExpression cs es _) = "let " ++ show cs ++ " in " ++ show es ++ " end"

instance HasSourceRegion Expression where
  sourceRegion (NilExpression r) = r
  sourceRegion (IntExpression _ r) = r
  sourceRegion (StringExpression _ r) = r
  sourceRegion (ArrayCreationExpression _ _ _ r) = r
  sourceRegion (RecordCreationExpression _ _ r) = r
  sourceRegion (LValueExpression _ r) = r
  sourceRegion (CallExpression _ _ r) = r
  sourceRegion (NegateExpression _ r) = r
  sourceRegion (OpExpression _ _ _ r) = r
  sourceRegion (SeqExpression _ r) = r
  sourceRegion (AssignmentExpression _ _ r) = r
  sourceRegion (IfExpression _ _ _ r) = r
  sourceRegion (WhileExpression _ _ r) = r
  sourceRegion (ForExpression _ _ _ _ r) = r
  sourceRegion (BreakExpression r) = r
  sourceRegion (LetExpression _ _ r) = r

data Chunk
  = TypeDecl TypeIdentifier Type !SourceRegion
  | FuncDecl Identifier [TypedField] (Maybe TypeIdentifier) Expression !SourceRegion
  | PrimitiveDecl Identifier [TypedField] (Maybe TypeIdentifier) !SourceRegion
  | VarDecl Identifier (Maybe TypeIdentifier) Expression !SourceRegion

instance Show Chunk where
  show (TypeDecl ti t _) = "type " ++ show ti ++ " = " ++ show t
  show (FuncDecl i fs r e _) = "function " ++ show i ++ "(" ++ show fs ++ ")" ++ maybe "" (\r' -> " : " ++ show r') r ++ " = " ++ show e
  show (PrimitiveDecl i fs r _) = "primitive " ++ show i ++ "(" ++ show fs ++ ")" ++ maybe "" (\r' -> " : " ++ show r') r
  show (VarDecl i t e _) = "var " ++ show i ++ maybe "" (\t' -> " : " ++ show t') t ++ " := " ++ show e

instance Eq Chunk where
  (TypeDecl ti1 t1 _) == (TypeDecl ti2 t2 _) = ti1 == ti2 && t1 == t2
  (FuncDecl i1 fs1 r1 e1 _) == (FuncDecl i2 fs2 r2 e2 _) = i1 == i2 && fs1 == fs2 && r1 == r2 && e1 == e2
  (PrimitiveDecl i1 fs1 r1 _) == (PrimitiveDecl i2 fs2 r2 _) = i1 == i2 && fs1 == fs2 && r1 == r2
  (VarDecl i1 t1 e1 _) == (VarDecl i2 t2 e2 _) = i1 == i2 && t1 == t2 && e1 == e2

instance HasSourceRegion Chunk where
  sourceRegion (TypeDecl _ _ r) = r
  sourceRegion (FuncDecl _ _ _ _ r) = r
  sourceRegion (PrimitiveDecl _ _ _ r) = r
  sourceRegion (VarDecl _ _ _ r) = r

data Program
  = ExpressionProgram Expression !SourceRegion
  | Chunks [Chunk] !SourceRegion

instance Eq Program where
  (ExpressionProgram e1 _) == (ExpressionProgram e2 _) = e1 == e2
  (Chunks cs1 _) == (Chunks cs2 _) = cs1 == cs2
  _ == _ = False

instance Show Program where
  show (ExpressionProgram e _) = show e
  show (Chunks cs _) = show cs

instance HasSourceRegion Program where
  sourceRegion (ExpressionProgram _ r) = r
  sourceRegion (Chunks _ r) = r