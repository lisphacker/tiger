module Tiger.Syntax.AST where

import Data.List (intercalate)
import Data.Text (Text, unpack)

class ASTNode a

printToken :: String -> String -> String
printToken _ a = a

-- printToken s a = s ++ "<" ++ show a ++ ">"

data Identifier a = Identifier Text a

instance Eq (Identifier a) where
  (Identifier s1 _) == (Identifier s2 _) = s1 == s2

instance Show (Identifier a) where
  show (Identifier s _) = printToken "ID" $ unpack s

type TypeIdentifier = Identifier

data TypedField a = TypedField (Identifier a) (TypeIdentifier a) a

instance Eq (TypedField a) where
  (TypedField i1 ti1 _) == (TypedField i2 ti2 _) = i1 == i2 && ti1 == ti2

instance Show (TypedField a) where
  show (TypedField i t _) = show i ++ " : " ++ show t

data Type a
  = TypeAlias (TypeIdentifier a) a
  | RecordType [TypedField a] a
  | ArrayType (TypeIdentifier a) a

instance Eq (Type a) where
  (TypeAlias ti1 _) == (TypeAlias ti2 _) = ti1 == ti2
  (RecordType fs1 _) == (RecordType fs2 _) = fs1 == fs2
  (ArrayType ti1 _) == (ArrayType ti2 _) = ti1 == ti2
  _ == _ = False

instance Show (Type a) where
  show (TypeAlias ti _) = show ti
  show (RecordType fs _) = "{" ++ fieldsStr ++ "}"
   where
    fieldsStr = intercalate "," $ map (\(TypedField i t _) -> show i ++ " : " ++ show t) fs
  show (ArrayType ti _) = "array of " ++ show ti

data LValue a
  = IdLValue (Identifier a) a
  | RecordLValue (LValue a) (Identifier a) a
  | ArrayLValue (LValue a) (Expression a) a

instance Eq (LValue a) where
  (IdLValue i1 _) == (IdLValue i2 _) = i1 == i2
  (RecordLValue l1 i1 _) == (RecordLValue l2 i2 _) = l1 == l2 && i1 == i2
  (ArrayLValue l1 e1 _) == (ArrayLValue l2 e2 _) = l1 == l2 && e1 == e2
  _ == _ = False

instance Show (LValue a) where
  show (IdLValue i _) = show i
  show (RecordLValue l i _) = show l ++ "." ++ show i
  show (ArrayLValue l e _) = show l ++ "[" ++ show e ++ "]"

data BinaryOperator
  = AddOp
  | SubOp
  | MulOp
  | DivOp
  | EqOp
  | NeqOp
  | LtOp
  | LeOp
  | GtOp
  | GeOp
  | AndOp
  | OrOp
  deriving (Eq)

-- instance Eq BinaryOperator where
--   AddOp == AddOp = True
--   SubOp == SubOp = True
--   MulOp == MulOp = True
--   DivOp == DivOp = True
--   EqOp == EqOp = True
--   NeqOp == NeqOp = True
--   LtOp == LtOp = True
--   LeOp == LeOp = True
--   GtOp == GtOp = True
--   GeOp == GeOp = True
--   AndOp == AndOp = True
--   OrOp == OrOp = True
--   _ == _ = False

instance Show BinaryOperator where
  show AddOp = "+"
  show SubOp = "-"
  show MulOp = "*"
  show DivOp = "/"
  show EqOp = "="
  show NeqOp = "<>"
  show LtOp = "<"
  show LeOp = "<="
  show GtOp = ">"
  show GeOp = ">="
  show AndOp = "&"
  show OrOp = "|"

data Expression a
  = NilExpression a
  | IntExpression Int a
  | StringExpression Text a
  | ArrayCreationExpression (TypeIdentifier a) (Expression a) (Expression a) a
  | RecordCreationExpression (TypeIdentifier a) [(Identifier a, Expression a)] a
  | LValueExpression (LValue a) a
  | CallExpression (Identifier a) [Expression a] a
  | NegateExpression (Expression a) a
  | OpExpression BinaryOperator (Expression a) (Expression a) a
  | SeqExpression [Expression a] a
  | AssignmentExpression (LValue a) (Expression a) a
  | IfExpression (Expression a) (Expression a) (Maybe (Expression a)) a
  | WhileExpression (Expression a) (Expression a) a
  | ForExpression (Identifier a) (Expression a) (Expression a) (Expression a) a
  | BreakExpression a
  | LetExpression [Decl a] [Expression a] a

instance Eq (Expression a) where
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

instance Show (Expression a) where
  show (NilExpression _) = "nil"
  show (IntExpression i _) = printToken "INT" $ show i
  show (StringExpression s _) = show s
  show (ArrayCreationExpression t s v _) = show t ++ "[" ++ show s ++ "] of " ++ show v
  show (RecordCreationExpression t fs _) = show t ++ "{" ++ fieldsStr ++ "}"
   where
    fieldsStr = intercalate "," $ map (\(i, e) -> show i ++ " = " ++ show e) fs
  show (LValueExpression l _) = printToken "LVALUE" $ show l
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

data Decl a
  = TypeDecl (TypeIdentifier a) (Type a) a
  | FuncDecl (Identifier a) [TypedField a] (Maybe (TypeIdentifier a)) (Expression a) a
  | VarDecl (Identifier a) (Maybe (TypeIdentifier a)) (Expression a) a

instance Show (Decl a) where
  show (TypeDecl ti t _) = printToken "TYPEDECL" $ "type " ++ show ti ++ " = " ++ show t
  show (FuncDecl i fs r e _) = printToken "FUNCDECL" $ "function " ++ show i ++ "(" ++ show fs ++ ")" ++ maybe "" (\r' -> " : " ++ show r') r ++ " = " ++ show e
  show (VarDecl i t e _) = printToken "VARDECL" $ "var " ++ show i ++ maybe "" (\t' -> " : " ++ show t') t ++ " := " ++ show e

instance Eq (Decl a) where
  (TypeDecl ti1 t1 _) == (TypeDecl ti2 t2 _) = ti1 == ti2 && t1 == t2
  (FuncDecl i1 fs1 r1 e1 _) == (FuncDecl i2 fs2 r2 e2 _) = i1 == i2 && fs1 == fs2 && r1 == r2 && e1 == e2
  (VarDecl i1 t1 e1 _) == (VarDecl i2 t2 e2 _) = i1 == i2 && t1 == t2 && e1 == e2
  _ == _ = False
