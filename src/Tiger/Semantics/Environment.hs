module Tiger.Semantics.Environment where

import Control.Applicative (Alternative (empty))
import Data.Map qualified as M
import Tiger.Semantics.Types qualified as T
import Tiger.Util.SourcePos (SourceSpan (..))
import Tiger.Util.SymbolTable (Symbol, SymbolTable)

data Environment = Environment
  { typeEnv :: SymbolTable (T.Type SourceSpan)
  , varEnv :: SymbolTable (T.Type SourceSpan)
  , parentEnv :: Maybe Environment
  }
  deriving (Eq)

baseEnv :: Environment
baseEnv = Environment M.empty M.empty Nothing

emptyEnv :: Environment
emptyEnv = Environment M.empty M.empty Nothing

lookupType :: Symbol -> Environment -> Maybe (T.Type SourceSpan)
lookupType s (Environment typeEnv _ _) = M.lookup s typeEnv

lookupVar :: Symbol -> Environment -> Maybe (T.Type SourceSpan)
lookupVar s (Environment _ varEnv _) = M.lookup s varEnv

insertType :: Symbol -> T.Type SourceSpan -> Environment -> Environment
insertType s t (Environment typeEnv varEnv parentEnv) = Environment (M.insert s t typeEnv) varEnv parentEnv

insertVar :: Symbol -> T.Type SourceSpan -> Environment -> Environment
insertVar s t (Environment typeEnv varEnv parentEnv) = Environment typeEnv (M.insert s t varEnv) parentEnv
