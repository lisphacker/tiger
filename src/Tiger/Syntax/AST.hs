module Tiger.Syntax.AST where

import Data.Text (Text)

data Identifier = Identifier Text
  deriving (Eq, Show)

type TypeIdentifier = Identifier

data LValue
  = IdLValue Identifier
  | RecordLValue LValue Identifier
  | ArrayLValue LValue Expression
  deriving (Eq, Show)

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
  deriving (Eq, Show)

data Expression
  = NilExpression
  | VarExpression Identifier
  | IntExpression Int
  | StringExpression Text
  | LValueExpression LValue
  | CallExpression Identifier [Expression]
  | NegateExpression Expression
  | OpExpression BinaryOperator Expression Expression
  | AssignmentExpression LValue Expression
  | IfExpression Expression Expression (Maybe Expression)
  | WhileExpression Expression Expression
  | ForExpression Identifier Expression Expression Expression
  | BreakExpression
  | LetExpression [Chunk] [Expression]
  deriving (Eq, Show)
data TypedField = TypedField Identifier TypeIdentifier
  deriving (Eq, Show)

data Type
  = TypeAlias TypeIdentifier
  | RecordType [TypedField]
  | ArrayType TypeIdentifier
  deriving (Eq, Show)

data VarDec = VarDec Identifier (Maybe TypeIdentifier) Exp
  deriving (Eq, Show)

data TypeDec = TypeDec TypeIdentifier Type
  deriving (Eq, Show)

data FuncDec
  = FuncDec Identifier [TypedField] (Maybe TypeIdentifier) Exp
  | Primitive Identifier [TypedField] (Maybe TypeIdentifier)
  deriving (Eq, Show)

data Chunk
  = TypeDecChunk [TypeDec]
  | FunDecChunk [FuncDec]
  | VarDecChunk VarDec
  deriving (Eq, Show)

data Program
  = ExpProgram Exp
  | Chunks [Chunk]
  deriving (Eq, Show)