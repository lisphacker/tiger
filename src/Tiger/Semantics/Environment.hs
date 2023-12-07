module Tiger.Semantics.Environment where

import Control.Applicative (Alternative (empty))
import Data.Map qualified as M
import Tiger.Semantics.Types qualified as T
import Tiger.Util.SourcePos (SourceLocation (SourceLocation), SourceSpan (..))
import Tiger.Util.SymbolTable (Symbol, SymbolTable)

data Environment = Environment
  { typeEnv :: SymbolTable T.Type
  , varEnv :: SymbolTable T.Type
  , parentEnv :: Maybe Environment
  }
  deriving (Eq)

baseTypeEnv :: SymbolTable T.Type
baseTypeEnv =
  M.fromList
    [ ("int", T.Int)
    , ("string", T.String)
    , ("nil", T.Nil)
    ]

__ :: SourceSpan
__ = SourceSpan (SourceLocation 0 0 0) (SourceLocation 0 0 0)

baseVarEnv :: SymbolTable T.Type
baseVarEnv =
  M.fromList
    [ ("print", T.FunCall [T.String] T.Unit 0 __)
    , ("flush", T.FunCall [] T.Unit 1 __)
    , ("getchar", T.FunCall [] T.String 2 __)
    , ("ord", T.FunCall [T.String] T.Int 3 __)
    , ("chr", T.FunCall [T.Int] T.String 4 __)
    , ("size", T.FunCall [T.String] T.Int 5 __)
    , ("substring", T.FunCall [T.String, T.Int, T.Int] T.String 6 __)
    , ("concat", T.FunCall [T.String, T.String] T.String 7 __)
    , ("not", T.FunCall [T.Int] T.Int 8 __)
    , ("exit", T.FunCall [T.Int] T.Unit 9 __)
    ]

baseEnv :: Environment
baseEnv = Environment baseTypeEnv baseVarEnv Nothing

newEnv :: Environment -> Environment
newEnv p = Environment M.empty M.empty $ Just p

lookupType :: Symbol -> Environment -> Maybe T.Type
lookupType s (Environment typeEnv _ parentEnv) =
  case M.lookup s typeEnv of
    Just t -> Just t
    Nothing -> case parentEnv of
      Just p -> lookupType s p
      Nothing -> Nothing

lookupVar :: Symbol -> Environment -> Maybe T.Type
lookupVar s (Environment _ varEnv parentEnv) =
  case M.lookup s varEnv of
    Just t -> Just t
    Nothing -> case parentEnv of
      Just p -> lookupVar s p
      Nothing -> Nothing

insertType :: Symbol -> T.Type -> Environment -> Environment
insertType s t (Environment typeEnv varEnv parentEnv) = Environment (M.insert s t typeEnv) varEnv parentEnv

insertVar :: Symbol -> T.Type -> Environment -> Environment
insertVar s t (Environment typeEnv varEnv parentEnv) = Environment typeEnv (M.insert s t varEnv) parentEnv
