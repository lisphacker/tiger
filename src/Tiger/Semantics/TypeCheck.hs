module Tiger.Semantics.TypeCheck where

import Control.Monad.State (MonadState (..), State, evalState)
import Tiger.Semantics.Environment qualified as E
import Tiger.Semantics.Types qualified as T
import Tiger.Syntax.AST qualified as AST

newtype TypeCheckState = TypeCheckState Int

incGetUid :: State TypeCheckState Int
incGetUid = do
  (TypeCheckState uid) <- get
  let uid' = uid + 1
  put $ TypeCheckState uid'
  pure uid'

typeCheckExp :: E.Environment -> AST.Expression a -> Either String (T.Type a)
typeCheckExp env expr = evalState (typeCheckExpST env expr) (TypeCheckState 0)

typeCheckTypeST :: E.Environment -> AST.TypeIdentifier a -> State TypeCheckState (Either String (T.Type a))
typeCheckTypeST = undefined

typeCheckExpST :: E.Environment -> AST.Expression a -> State TypeCheckState (Either String (T.Type a))
typeCheckExpST e@(E.Environment typeEnv varEnv _) expr =
  case expr of
    (AST.NilExpression p) -> pure $ Right T.Nil
    (AST.IntExpression p _) -> pure $ Right T.Int
    (AST.StringExpression p _) -> pure $ Right T.String
    (AST.ArrayCreationExpression ti _ _ p) -> do
      ti' <- typeCheckTypeST e ti
      uid <- incGetUid
      pure $ case ti' of
        Right t -> Right $ T.Array t uid p
        Right _ -> Left "Array type must be an alias"
        Left e -> Left e