{-# LANGUAGE InstanceSigs #-}

module Tiger.Semantics.Types where

import Tiger.Util.SymbolTable (Symbol, Uid)

data Type a
  = Int
  | String
  | Nil
  | Record [(Symbol, Type a)] Uid a
  | Array (Type a) Uid a
  | FunCall [Type a] (Type a) Uid a
  | UnresolvedType Symbol

instance Eq (Type a) where
  (==) :: Type a -> Type a -> Bool
  Int == Int = True
  String == String = True
  Nil == Nil = True
  Record _ u _ == Record _ u' _ = u == u'
  Array _ u _ == Array _ u' _ = u == u'
  FunCall _ _ u _ == FunCall _ _ u' _ = u == u'
  _ == _ = False
