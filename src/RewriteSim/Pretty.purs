module RewriteSim.Pretty where

import Prelude

import Data.Foldable (class Foldable, intercalate)
import Data.List (List)
import Data.Map (Map)
import Data.Map as Map
import Data.Tuple (Tuple(..))

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

prettyMap :: forall k v. (k -> String) -> (v -> String) -> Map k v -> String
prettyMap pk pv m = "{{ " <> ((m # Map.toUnfoldable :: List _) # map (\(Tuple k v) -> pk k <> " -> " <> pv v) # intercalate " ,, ") <> " }}"

prettyFoldable :: forall f x. Functor f => Foldable f => (x -> String) -> f x -> String
prettyFoldable fx xs = "[[ " <> intercalate " ,, " (map fx xs) <> " ]]"

metaquotes :: String -> String
metaquotes s = "{{ " <> s <> " }}"
