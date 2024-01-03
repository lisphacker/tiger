module Tiger.Semantics.TypeCheck where

import Control.Monad.State (MonadState (..), State, evalState, foldM)
import Data.Maybe (isJust)
import Data.Text (unpack)
import GHC.Base (Type, undefined)
import Tiger.Errors (Error (TypeError), Result)
import Tiger.Semantics.Environment (newEnv)
import Tiger.Semantics.Environment qualified as E
import Tiger.Semantics.Types qualified as T
import Tiger.Syntax.AST qualified as AST
import Tiger.Syntax.Parser
import Tiger.Syntax.Tokens (Token (Do))
import Tiger.Util.SourcePos (SourceSpan, Spanned (..), emptySpan)
import Tiger.Util.SymbolTable (Symbol)

isRight (Right _) = True
isRight _ = False

fromRight (Right t) = t
fromRight _ = undefined

newtype TypeCheckState = TypeCheckState Int
type WithSemanticState = State TypeCheckState

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

getIncrementedUid :: WithSemanticState Int
getIncrementedUid = do
  (TypeCheckState uid) <- get
  let uid' = uid + 1
  put $ TypeCheckState uid'
  pure uid'

typeCheckTypeIdentifierST :: E.Environment -> AST.TypeIdentifier SourceSpan -> WithSemanticState (Result T.Type)
typeCheckTypeIdentifierST e ti =
  case lookupType ti e of
    Just (T.NamedType s _) -> typeCheckTypeIdentifierST e (AST.Identifier s emptySpan)
    Just t -> pure $ Right t
    Nothing -> pure $ Left $ TypeError "Unresolved type" (getSpan ti)

typeCheckTypedFieldST :: E.Environment -> AST.TypedField SourceSpan -> WithSemanticState ((Symbol, T.Type), E.Environment)
typeCheckTypedFieldST e (AST.TypedField id@(AST.Identifier idn _) tid@(AST.Identifier tidn _) sp) = do
  eiType <- typeCheckTypeIdentifierST e tid
  case eiType of
    Right t -> pure ((idn, t), e)
    Left err -> do
      let t = T.NamedType tidn Nothing
      let e' = E.insertType tidn t e
      pure ((idn, t), e')

typeCheckTypeST :: E.Environment -> AST.Type SourceSpan -> WithSemanticState (Result (T.Type, E.Environment))
typeCheckTypeST e (AST.TypeAlias ti@(AST.Identifier tin sp) _) = do
  case lookupType ti e of
    Just (T.NamedType s _) -> do
      let t = T.NamedType s Nothing
      typeCheckTypeST e (AST.TypeAlias (AST.Identifier s sp) sp)
    Just ty -> pure $ Right (ty, e)
    Nothing -> do
      let ty = T.NamedType tin Nothing
      let e' = E.insertType tin ty e
      pure $ Right (ty, e')
typeCheckTypeST e (AST.RecordType fs sp) = do
  (symFtys, e') <- foldM f ([], e) fs
  uid <- getIncrementedUid
  let ty = T.Record symFtys uid sp
  pure $ Right (ty, e')
 where
  f :: ([(Symbol, T.Type)], E.Environment) -> AST.TypedField SourceSpan -> WithSemanticState ([(Symbol, T.Type)], E.Environment)
  f (symtys, e) (AST.TypedField id@(AST.Identifier idn _) ti _) = do
    eiType <- typeCheckTypeIdentifierST e ti
    case eiType of
      Right t -> pure ((idn, t) : symtys, e)
      Left err -> do
        let t = T.NamedType idn Nothing
        let e' = E.insertType idn t e
        pure ((idn, t) : symtys, e')
typeCheckTypeST e (AST.ArrayType ti@(AST.Identifier tin _) sp) = do
  eiType <- typeCheckTypeIdentifierST e ti
  case eiType of
    Right t -> do
      uid <- getIncrementedUid
      let ty = T.Array t uid sp
      pure $ Right (ty, e)
    Left err -> do
      let t = T.NamedType tin Nothing
      let e' = E.insertType tin t e
      uid <- getIncrementedUid
      let ty = T.Array t uid sp
      pure $ Right (ty, e')

processDeclST :: E.Environment -> AST.Decl SourceSpan -> WithSemanticState (Result E.Environment)
processDeclST e (AST.TypeDecl (AST.Identifier ti _) astTy _) = do
  eiType <- typeCheckTypeST e astTy
  case eiType of
    Right (ty, e') -> pure $ Right $ E.insertType ti ty e'
    Left err -> pure $ Left err
processDeclST e (AST.VarDecl id@(AST.Identifier idn _) (Just tid@(AST.Identifier tidn _)) expr _) = do
  eiType <- typeCheckTypeIdentifierST e tid
  case eiType of
    Right t -> do
      eiExprType <- typeCheckExprST e expr
      case eiExprType of
        Right t' ->
          if t == t'
            then pure $ Right $ E.insertVar idn t e
            else pure $ Left $ TypeError "Expected same type" (getSpan expr)
        Left err -> pure $ Left err
    Left err -> pure $ Left err
processDeclST e (AST.VarDecl id@(AST.Identifier idn _) Nothing expr _) = do
  eiExprType <- typeCheckExprST e expr
  case eiExprType of
    Right t -> pure $ Right $ E.insertVar idn t e
    Left err -> pure $ Left err

processDeclsST :: E.Environment -> [AST.Decl SourceSpan] -> WithSemanticState (Result E.Environment)
processDeclsST e ds = do
  let typeDecs = filter isTypeDecl ds
  let varDecs = filter isVarDecl ds
  let funcDecs = filter isFuncDecl ds
  e' <- processDeclsST' (Right e) typeDecs
  e'' <- processDeclsST' e' varDecs
  processDeclsST' e'' funcDecs
 where
  isTypeDecl (AST.TypeDecl{}) = True
  isTypeDecl _ = False
  isVarDecl (AST.VarDecl{}) = True
  isVarDecl _ = False
  isFuncDecl (AST.FuncDecl{}) = True
  isFuncDecl _ = False
  processDeclsST' :: Result E.Environment -> [AST.Decl SourceSpan] -> WithSemanticState (Result E.Environment)
  processDeclsST' (Left err) _ = pure $ Left err
  processDeclsST' (Right e) [] = pure $ Right e
  processDeclsST' (Right e) (d : ds) = do
    e' <- processDeclST e d
    case e' of
      Right e'' -> processDeclsST' (Right e'') ds
      Left err -> pure $ Left err

typeCheckExprST :: E.Environment -> AST.Expression SourceSpan -> WithSemanticState (Result T.Type)
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
          pure $
            if map Right argTypes == argTypes'
              then Right retType
              else Left $ TypeError "Argument type mismatch" p
        Just _ -> pure $ Left $ TypeError "Expected function type" p
        Nothing -> pure $ Left $ TypeError ("Unresolved function '" ++ unpack idn ++ "'") p
    (AST.NegateExpression expr p) -> typeCheckExprST e expr
    (AST.OpExpression op expr1 expr2 p) -> do
      eiT1 <- typeCheckExprST e expr1
      eiT2 <- typeCheckExprST e expr2
      case (eiT1, eiT2) of
        (Right t1, Right t2) ->
          if op `elem` [AST.AddOp, AST.SubOp, AST.MulOp, AST.DivOp]
            then
              if t1 == T.Int && t2 == T.Int
                then pure $ Right T.Int
                else pure $ Left $ TypeError "Expected integer type" p
            else
              if op `elem` [AST.EqOp, AST.NeqOp, AST.LtOp, AST.LeOp, AST.GtOp, AST.GeOp]
                then
                  if t1 == t2
                    then pure $ Right t1
                    else pure $ Left $ TypeError "Expected same type" p
                else
                  if op `elem` [AST.AndOp, AST.OrOp]
                    then
                      if t1 == T.Bool && t2 == T.Bool
                        then pure $ Right T.Bool
                        else pure $ Left $ TypeError "Expected boolean type" p
                    else pure $ Left $ TypeError "Unresolved operator" p
        (Left err, _) -> pure $ Left err
        (_, Left err) -> pure $ Left err
    (AST.SeqExpression exprs p) -> do
      ets <- mapM (typeCheckExprST e) exprs
      pure $
        if all isRight ets
          then Right $ last $ map fromRight ets
          else Left $ TypeError "Expected same type" p
    (AST.AssignmentExpression lval expr p) -> do
      eiT1 <- typeCheckExprST e (AST.LValueExpression lval p)
      eiT2 <- typeCheckExprST e expr
      case (eiT1, eiT2) of
        (Right t1, Right t2) ->
          if t1 == t2
            then pure $ Right T.Unit
            else pure $ Left $ TypeError "Expected same type" p
        (Left err, _) -> pure $ Left err
        (_, Left err) -> pure $ Left err
    (AST.IfExpression cond expr1 (Just expr2) p) -> do
      eiT1 <- typeCheckExprST e cond
      eiT2 <- typeCheckExprST e expr1
      eiT3 <- typeCheckExprST e expr2
      case (eiT1, eiT2, eiT3) of
        (Right t1, Right t2, Right t3) ->
          if t1 == T.Bool && t2 == t3
            then pure $ Right t2
            else pure $ Left $ TypeError "Expected boolean condition and same type for then and else" p
        (Left err, _, _) -> pure $ Left err
        (_, Left err, _) -> pure $ Left err
        (_, _, Left err) -> pure $ Left err
    (AST.IfExpression cond expr Nothing p) -> do
      eiT1 <- typeCheckExprST e cond
      eiT2 <- typeCheckExprST e expr
      case (eiT1, eiT2) of
        (Right t1, Right t2) ->
          if t1 == T.Bool
            then pure $ Right T.Unit
            else pure $ Left $ TypeError "Expected boolean condition and same type for then and else" p
        (Left err, _) -> pure $ Left err
        (_, Left err) -> pure $ Left err
    (AST.WhileExpression cond expr p) -> do
      eiT1 <- typeCheckExprST e cond
      eiT2 <- typeCheckExprST e expr
      case (eiT1, eiT2) of
        (Right t1, Right t2) ->
          if t1 == T.Bool && t2 == T.Unit
            then pure $ Right T.Unit
            else pure $ Left $ TypeError "Expected boolean condition and unit body" p
        (Left err, _) -> pure $ Left err
        (_, Left err) -> pure $ Left err
    (AST.ForExpression id@(AST.Identifier idn _) expr1 expr2 expr3 p) -> do
      eiT1 <- typeCheckExprST e expr1
      eiT2 <- typeCheckExprST e expr2
      eiT3 <- typeCheckExprST e expr3
      case (eiT1, eiT2, eiT3) of
        (Right t1, Right t2, Right t3) ->
          if t1 == T.Int && t2 == T.Int && t3 == T.Unit
            then pure $ Right T.Unit
            else pure $ Left $ TypeError "Expected integer type for bounds and unit body" p
        (Left err, _, _) -> pure $ Left err
        (_, Left err, _) -> pure $ Left err
        (_, _, Left err) -> pure $ Left err
    (AST.BreakExpression p) -> pure $ Right T.Unit
    (AST.LetExpression decls exprs p) -> do
      let e' = newEnv e
      eitherE'' <- processDeclsST e' decls
      case eitherE'' of
        Right e'' -> do
          ets <- mapM (typeCheckExprST e'') exprs
          pure $
            if all isRight ets
              then Right $ last $ map fromRight ets
              else Left $ TypeError "Expected same type" p
        Left err -> pure $ Left err

typeCheckExpr :: E.Environment -> AST.Expression SourceSpan -> Result T.Type
typeCheckExpr env expr = evalState (typeCheckExprST env expr) (TypeCheckState 100)
