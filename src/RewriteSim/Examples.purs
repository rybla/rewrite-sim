module RewriteSim.Examples where

import Prelude

import Data.Map (Map)
import Data.Map as Map
import Data.Tuple.Nested ((/\))
import RewriteSim.Example.ABC as ABC
import RewriteSim.Example.Common (Example, ExampleTest, System)
import RewriteSim.Example.ISLC as ISLC
import RewriteSim.Example.LC as LC

examples :: Array Example
examples =
  [ LC.example
  , ISLC.example
  , ABC.example
  ]

exampleExprs :: Map String (Array ExampleTest)
exampleExprs = examples
  # map (\example -> (example.name /\ example.tests))
  # Map.fromFoldable

exampleSystems :: Array System
exampleSystems = examples
  # map (\example -> { name: example.name, rules: example.rules })

