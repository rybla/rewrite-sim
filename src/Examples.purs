module Examples where

import Prelude

import Example.Common (Expr, Rule, System)
import Data.Map (Map)
import Data.Map as Map
import Data.Tuple.Nested (type (/\), (/\))
import Example.ABC as ABC
import Example.LC as LC

examples
  :: Array
       ( String /\
           { rules :: Array Rule
           , exprs :: Array (String /\ Expr)
           }
       )
examples =
  [ LC.example
  , ABC.example
  ]

exampleExprs :: Map String (Array (String /\ Expr))
exampleExprs = examples
  # map (\(systemName /\ x) -> (systemName /\ x.exprs))
  # Map.fromFoldable

exampleSystems :: Array System
exampleSystems = examples
  # map (\(systemName /\ x) -> { name: systemName, rules: x.rules })

