{-# LANGUAGE InstanceSigs #-}

module Tiger.Semantics.Types where

import Tiger.Util.SourcePos (SourceSpan)
import Tiger.Util.SymbolTable (Symbol, Uid)

data Type
  = Int
  | String
  | Bool
  | Nil
  | Unit
  | Record [(Symbol, Type)] Uid SourceSpan
  | Array Type Uid SourceSpan
  | FunCall [Type] Type Uid SourceSpan
  | NamedType Symbol (Maybe Type)

instance Eq Type where
  (==) :: Type -> Type -> Bool
  Int == Int = True
  String == String = True
  Nil == Nil = True
  Record _ u _ == Record _ u' _ = u == u'
  Array _ u _ == Array _ u' _ = u == u'
  FunCall _ _ u _ == FunCall _ _ u' _ = u == u'
  _ == _ = False
