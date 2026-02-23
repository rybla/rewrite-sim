module Main where

import Prelude

import Control.Monad.Reader (runReaderT)
import Control.Monad.State (get, modify_)
import Data.Array as Array
import Data.List (List)
import Data.List as List
import Data.Maybe (Maybe(..), fromMaybe')
import Data.Tuple (snd)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Unfoldable (none)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML (PlainHTML)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver as HVD
import Partial.Unsafe (unsafeCrashWith)
import RewriteSim ((%))
import RewriteSim as RS

----------------
-- main
----------------

main :: Effect Unit
main = HA.runHalogenAff (HVD.runUI appComponent {} =<< HA.awaitBody)

----------------
-- appComponent
----------------

type Expr = RS.Expr String
type Rule = RS.Rule String
type System = RS.System String

type AppState =
  { expr :: Expr
  , system :: System
  , history :: List Expr
  , messages :: List PlainHTML
  }

data AppAction
  = StepForwardExpr
  | StepBackwardExpr
  | SetExpr Expr
  | SetSystem System

appComponent :: forall query input output m. MonadAff m => H.Component query input output m
appComponent = H.mkComponent { initialState, eval, render }
  where
  initialState :: input -> AppState
  initialState _ =
    { expr: exampleExprs Array.!! 0 # fromMaybe' (\_ -> unsafeCrashWith "empty exampleExprs") # snd
    , system: exampleSystems Array.!! 0 # fromMaybe' (\_ -> unsafeCrashWith "empty exampleSystems")
    , history: none
    , messages: none
    }

  eval = H.mkEval H.defaultEval
    { handleAction = case _ of
        StepForwardExpr -> do
          { expr, system } <- get
          mb_expr' <- RS.step expr
            # flip runReaderT { system }
          case mb_expr' of
            Nothing -> do
              modify_ \state -> state
                { messages = List.Cons
                    ( HH.span_
                        [ HH.text "❌ StepForwardExpr: idempotent" ]
                    )
                    state.messages
                }
            Just expr' -> do
              modify_ \state -> state
                { messages = List.Cons
                    ( HH.span_
                        [ HH.text "✅ StepForwardExpr: updated" ]
                    )
                    state.messages
                , expr = expr'
                , history = List.Cons expr state.history
                }
          pure unit
        StepBackwardExpr -> do
          { history } <- get
          case history of
            List.Nil -> do
              modify_ \state -> state
                { messages =
                    List.Cons
                      (HH.span_ [ HH.text "❌ StepBackwardExpr: empty history" ])
                      state.messages
                }
            List.Cons expr history' -> do
              modify_ \state -> state
                { messages =
                    List.Cons
                      (HH.span_ [ HH.text "✅ StepBackwardExpr: updated" ])
                      state.messages
                , history = history'
                , expr = expr
                }
          pure unit
        SetExpr expr -> do
          modify_ \state -> state
            { messages = List.Cons
                ( HH.span_ $
                    Array.intercalate [ HH.text " " ]
                      [ [ HH.text "SetExpr" ]
                      , [ RS.renderExpr HH.text expr ]
                      ]
                )
                state.messages
            , history = none
            , expr = expr
            }
          pure unit
        SetSystem system -> do
          modify_ \state -> state
            { messages = List.Cons
                ( HH.span_ $
                    Array.intercalate [ HH.text " " ]
                      [ [ HH.text "SetSystem" ]
                      , [ HH.text system.name ]
                      ]
                )
                state.messages
            }
          pure unit
    }

  render state =
    HH.div [ HP.classes [ HH.ClassName "app" ] ]
      [ HH.div [ HP.classes [ HH.ClassName "title" ] ]
          [ HH.text "rewrite-sim" ]
      -- 
      , HH.div [ HP.classes [ HH.ClassName "menu" ] ]
          [ HH.div [ HP.classes [ HH.ClassName "menu-section" ] ]
              [ HH.div [ HP.classes [ HH.ClassName "menu-section-title" ] ]
                  [ HH.text "Systems" ]
              , HH.div [ HP.classes [ HH.ClassName "menu-section-items" ] ] $ exampleSystems # map \system ->
                  HH.button [ HE.onClick $ const $ SetSystem system ] [ HH.text system.name ]
              ]
          , HH.div [ HP.classes [ HH.ClassName "menu-section" ] ]
              [ HH.div [ HP.classes [ HH.ClassName "menu-section-title" ] ]
                  [ HH.text "Example Expressions" ]
              , HH.div [ HP.classes [ HH.ClassName "menu-section-items" ] ] $ exampleExprs # map \(label /\ expr) ->
                  HH.button [ HE.onClick $ const $ SetExpr expr ] [ HH.text label ]
              ]
          ]
      -- 
      , HH.div [ HP.classes [ HH.ClassName "controls" ] ]
          [ HH.button [ HE.onClick $ const $ StepBackwardExpr ] [ HH.text "Backward" ]
          , HH.button [ HE.onClick $ const $ StepForwardExpr ] [ HH.text "Forward" ]
          ]
      -- 
      , HH.div [ HP.classes [ HH.ClassName "view" ] ]
          [ RS.renderExpr HH.text state.expr ]
      -- 
      , HH.div [ HP.classes [ HH.ClassName "console" ] ]
          [ HH.div [ HP.classes [ HH.ClassName "console-title" ] ]
              [ HH.text "Console" ]
          , HH.div [ HP.classes [ HH.ClassName "console-items" ] ] $
              state.messages # Array.fromFoldable # map \message ->
                HH.div [ HP.classes [ HH.ClassName "console-message" ] ]
                  [ message # HH.fromPlainHTML ]
          ]
      ]

exampleExprs :: Array (String /\ Expr)
exampleExprs =
  [ "ex1" /\ ("app" % [ "lam" % [ "x" % [], "var" % [ "x" % [] ] ], "var" % [ "a" % [] ] ])
  ]

exampleSystems :: Array System
exampleSystems =
  [ { name: "LC"
    , rules:
        [ case _ of
            "app" % [ "lam" % [ x % [], b ], a ] -> pure $ subst x a b
            _ -> Nothing
        ]
    }
  ]

subst :: String -> Expr -> Expr -> Expr
subst x a ("var" % [ y % [] ]) | x == y = a
subst x a (l % es) = l % (es # map (subst x a))
