module Tiger.Semantics.TypeCheck where

import Control.Monad.State (MonadState (..), State, evalState)
import Data.Text (unpack)
import Tiger.Errors (Error (TypeError))
import Tiger.Semantics.Environment qualified as E
import Tiger.Semantics.Types qualified as T
import Tiger.Syntax.AST qualified as AST
import Tiger.Util.SourcePos (SourceSpan)

newtype TypeCheckState = TypeCheckState Int

lookupType :: AST.TypeIdentifier SourceSpan -> E.Environment -> Maybe T.Type
lookupType (AST.Identifier s _) e =
  case E.lookupType s e of
    Just (T.NamedType s' _) -> E.lookupType s' e
    Just t -> Just t
    Nothing -> Nothing

lookupVar :: AST.Identifier SourceSpan -> E.Environment -> Maybe T.Type
lookupVar (AST.Identifier s _) e =
  case E.lookupVar s e of
    Just (T.NamedType s' _) -> E.lookupType s' e
    Just t -> Just t
    Nothing -> Nothing

getIncrementedUid :: State TypeCheckState Int
getIncrementedUid = do
  (TypeCheckState uid) <- get
  let uid' = uid + 1
  put $ TypeCheckState uid'
  pure uid'

typeCheckExpr :: E.Environment -> AST.Expression SourceSpan -> Either Error T.Type
typeCheckExpr env expr = evalState (typeCheckExprST env expr) (TypeCheckState 100)

typeCheckTypeST :: E.Environment -> AST.TypeIdentifier SourceSpan -> State TypeCheckState (Either Error T.Type)
typeCheckTypeST = undefined

typeCheckExprST :: E.Environment -> AST.Expression SourceSpan -> State TypeCheckState (Either Error T.Type)
typeCheckExprST e@(E.Environment typeEnv varEnv _) expr =
  case expr of
    (AST.NilExpression p) -> pure $ Right T.Nil
    (AST.IntExpression p _) -> pure $ Right T.Int
    (AST.StringExpression p _) -> pure $ Right T.String
    (AST.ArrayCreationExpression ti _ _ p) -> do
      case lookupType ti e of
        Just ti'@(T.Array{}) -> pure $ Right ti'
        Just _ -> pure $ Left $ TypeError "Expected array type" p
        Nothing -> pure $ Left $ TypeError "Unresolved type" p
    (AST.RecordCreationExpression ti fs p) -> do
      case lookupType ti e of
        Just ti'@(T.Record{}) -> pure $ Right ti'
        Just _ -> pure $ Left $ TypeError "Expected record type" p
        Nothing -> pure $ Left $ TypeError "Unresolved type" p
    (AST.LValueExpression lval p) ->
      case lval of
        (AST.IdLValue (AST.Identifier id _) _) -> do
          case E.lookupVar id e of
            Just t -> pure $ Right t
            Nothing -> pure $ Left $ TypeError ("Unresolved variable '" ++ unpack id ++ "'") p
        (AST.RecordLValue lval (AST.Identifier id _) _) -> do
          case typeCheckExpr e (AST.LValueExpression lval p) of
            Right (T.Record fs _ _) -> do
              case lookup id fs of
                Just t -> pure $ Right t
                Nothing -> pure $ Left $ TypeError ("Unresolved field '" ++ unpack id ++ "'") p
            Right _ -> pure $ Left $ TypeError "Expected record type" p
            Left err -> pure $ Left err
        (AST.ArrayLValue lval expr _) -> do
          case typeCheckExpr e (AST.LValueExpression lval p) of
            Right (T.Array t _ _) -> do
              case typeCheckExpr e expr of
                Right _ -> pure $ Right t
                Left err -> pure $ Left err
            Right _ -> pure $ Left $ TypeError "Expected array type" p
            Left err -> pure $ Left err
    (AST.CallExpression id@(AST.Identifier idn _) args p) -> do
      case lookupVar id e of
        Just (T.FunCall argTypes retType _ _) -> do
          argTypes' <- mapM (typeCheckExprST e) args
          pure $ if map Right argTypes == argTypes' then 
                  Right retType
                else
                  Left $ TypeError "Argument type mismatch" p
        Just _ -> pure $ Left $ TypeError "Expected function type" p
        Nothing -> pure $ Left $ TypeError ("Unresolved function '" ++ unpack idn ++ "'") p
