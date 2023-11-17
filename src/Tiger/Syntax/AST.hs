module Tiger.Syntax.AST where

import Data.Text (Text, unpack)
import Tiger.Util.SourcePos (HasSourceRegion (..), SourceRegion)

data Identifier = Identifier Text !SourceRegion
  deriving (Eq)

instance Show Identifier where
  show (Identifier s _) = unpack s

instance HasSourceRegion Identifier where
  sourceRegion (Identifier _ r) = r

type TypeIdentifier = Identifier

class (HasSourceRegion a) => ASTNode a

data TypedField = TypedField Identifier TypeIdentifier !SourceRegion
  deriving (Eq, Show)

instance HasSourceRegion TypedField where
  sourceRegion (TypedField _ _ r) = r

data Type
  = TypeAlias TypeIdentifier !SourceRegion
  | RecordType [TypedField] !SourceRegion
  | ArrayType TypeIdentifier !SourceRegion
  deriving (Eq, Show)

instance HasSourceRegion Type where
  sourceRegion (TypeAlias _ r) = r
  sourceRegion (RecordType _ r) = r
  sourceRegion (ArrayType _ r) = r

data LValue
  = IdLValue Identifier !SourceRegion
  | RecordLValue LValue Identifier !SourceRegion
  | ArrayLValue LValue Expression !SourceRegion
  deriving (Eq)

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
  deriving (Eq)

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
  deriving (Eq)

instance Show Expression where
  show (NilExpression _) = "nil"
  show (IntExpression i _) = show i
  show (StringExpression s _) = show s
  show (ArrayCreationExpression t s v _) = show t ++ "[" ++ show s ++ "] of " ++ show v
  show (RecordCreationExpression t fs _) = show t ++ "{" ++ show fs ++ "}"
  show (LValueExpression l _) = show l
  show (CallExpression f args _) = show f ++ "(" ++ show args ++ ")"
  show (NegateExpression e _) = "(-(" ++ show e ++ "))"
  show (OpExpression op e1 e2 _) = "(" ++ show e1 ++ " " ++ show op ++ " " ++ show e2 ++ ")"
  show (SeqExpression es _) = show es
  show (AssignmentExpression l e _) = show l ++ " := " ++ show e
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
  deriving (Eq, Show)

instance HasSourceRegion Chunk where
  sourceRegion (TypeDecl _ _ r) = r
  sourceRegion (FuncDecl _ _ _ _ r) = r
  sourceRegion (PrimitiveDecl _ _ _ r) = r
  sourceRegion (VarDecl _ _ _ r) = r

data Program
  = ExpressionProgram Expression !SourceRegion
  | Chunks [Chunk] !SourceRegion
  deriving (Eq, Show)

instance HasSourceRegion Program where
  sourceRegion (ExpressionProgram _ r) = r
  sourceRegion (Chunks _ r) = r