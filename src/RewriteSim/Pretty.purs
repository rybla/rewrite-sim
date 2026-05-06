module RewriteSim.Pretty where

import Prelude

import Data.Foldable (intercalate)

class Pretty a where
  pretty :: a -> String

instance Pretty String where
  pretty = identity

instance Pretty Int where
  pretty = show

instance Pretty Boolean where
  pretty = show

instance Pretty a => Pretty (Array a) where
  pretty xs = "[" <> (xs # map pretty # intercalate ", ") <> "]"
