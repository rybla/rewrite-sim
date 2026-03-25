module RewriteSim.Examples where

import Prelude

import Data.Map (Map)
import Data.Map as Map
import Data.Tuple.Nested ((/\))
import RewriteSim.Example.ABC as ABC
import RewriteSim.Example.Common (Example, ExampleTest, System)
import RewriteSim.Example.ISLC as ISLC
import RewriteSim.Example.ISLSCv0d2d2 as ISLSCv0d2d2
import RewriteSim.Example.LC as LC

examples :: Array Example
examples =
  if false then
    [ LC.example
    , ABC.example
    , ISLC.example
    , ISLSCv0d2d2.example
    ]
  else
    [ ISLSCv0d2d2.example ]

exampleExprs :: Map String (Array ExampleTest)
exampleExprs = examples
  # map (\example -> (example.name /\ example.tests))
  # Map.fromFoldable

exampleSystems :: Array System
exampleSystems = examples
  # map (\example -> { name: example.name, rules: example.rules })

