module Tiger.Util.SymbolTable where

import Data.Map qualified as M
import Data.Text (Text)

type Uid = Int

type Symbol = Text
type SymbolTable = M.Map Symbol

symLookup :: Symbol -> SymbolTable a -> Maybe a
symLookup = M.lookup

symInsert :: Symbol -> a -> SymbolTable a -> SymbolTable a
symInsert = M.insert
