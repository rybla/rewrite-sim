module RewriteSim.Examples where

import Prelude

import Data.Map (Map)
import Data.Map as Map
import Data.Tuple.Nested (type (/\), (/\))
import RewriteSim.Example.ABC as ABC
import RewriteSim.Example.Common (Expr, Rule, System)
import RewriteSim.Example.ISLC as ISLC
import RewriteSim.Example.LC as LC

examples
  :: Array
       ( String /\
           { rules :: Array Rule
           , exprs :: Array (String /\ Expr)
           }
       )
examples =
  [ LC.example
  , ISLC.example
  , ABC.example
  ]

exampleExprs :: Map String (Array (String /\ Expr))
exampleExprs = examples
  # map (\(systemName /\ x) -> (systemName /\ x.exprs))
  # Map.fromFoldable

exampleSystems :: Array System
exampleSystems = examples
  # map (\(systemName /\ x) -> { name: systemName, rules: x.rules })

