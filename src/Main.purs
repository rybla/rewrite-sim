module Main where

import Prelude

import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (runReader, runReaderT)
import Control.Monad.State (get, modify_, runStateT)
import Control.Monad.Writer (runWriterT)
import Data.Array as Array
import Data.Foldable (fold, length)
import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe')
import Data.Tuple (Tuple(..), snd)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Unfoldable (none)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Halogen (HalogenM)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML (PlainHTML)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver as HVD
import Partial.Unsafe (unsafeCrashWith)
import Record as Record
import RewriteSim (normalize, (%))
import RewriteSim as RS
import Type.Proxy (Proxy(..))
import Utilities (runIdentity)

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
type GlobalUpdate = RS.GlobalUpdate String

type AppState = Record AppStateR
type AppStateR =
  ( system :: System
  , history :: List SimEvent
  , now :: SimEvent
  , future :: List SimEvent
  , messages :: List PlainHTML
  , focusNew :: Boolean
  )

data SimEvent
  = StaticExpr Expr
  | UpdateExpr GlobalUpdate

data AppAction
  = StepForwardExpr
  | StepBackwardExpr
  | ToggleFocus
  | SetExpr Expr
  | SetSystem System
  | ClearConsole

appComponent :: forall query input output m. MonadAff m => H.Component query input output m
appComponent = H.mkComponent { initialState, eval, render }
  where
  initialState :: input -> AppState
  initialState _ =
    let
      system = exampleSystems Array.!! 0 # fromMaybe' (\_ -> unsafeCrashWith "empty exampleSystems")
    in
      { messages: none }
        # setSystem system

  eval = H.mkEval H.defaultEval
    { handleAction = handleAction
    }

  handleAction :: forall slots. AppAction -> HalogenM AppState AppAction slots output m Unit
  handleAction = case _ of
    StepForwardExpr -> do
      { future } <- get
      case future of
        List.Nil -> do
          modify_ $ addMessage [ HH.text "❌ StepForward: empty future" ]
        List.Cons event future' -> do
          modify_ $ addMessage [ HH.text "✅ StepBackwardExpr: updated" ]
          modify_ \state -> state
            { history = List.Cons state.now state.history
            , now = event
            , future = future'
            }
      pure unit
    StepBackwardExpr -> do
      { history } <- get
      case history of
        List.Nil -> do
          modify_ $ addMessage [ HH.text "❌ StepBackwardExpr: empty history" ]
        List.Cons update history' -> do
          modify_ $ addMessage [ HH.text "✅ StepBackwardExpr: updated" ]
          modify_ \state -> state
            { history = history'
            , now = update
            , future = List.Cons state.now state.future
            }
      pure unit
    ToggleFocus -> do
      modify_ \state ->
        state
          { focusNew = not state.focusNew }
          # addMessage [ HH.text $ "toggled focus to " <> if state.focusNew then "old" else "new" ]
    SetExpr expr -> do
      modify_ $ setExpr expr
      pure unit
    SetSystem system -> do
      modify_ $ setSystem system
      pure unit
    ClearConsole -> modify_ _ { messages = none }

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
                  [ HH.text "Expressions" ]
              , HH.div [ HP.classes [ HH.ClassName "menu-section-items" ] ] $ exampleExprs # Map.lookup state.system.name # fold # map \(label /\ expr) ->
                  HH.button [ HE.onClick $ const $ SetExpr expr ] [ HH.text label ]
              ]
          ]
      -- 
      , HH.div [ HP.classes [ HH.ClassName "controls" ] ]
          [ HH.button [ HE.onClick $ const $ StepBackwardExpr ] [ HH.text "Backward" ]
          , HH.button [ HE.onClick $ const $ StepForwardExpr ] [ HH.text "Forward" ]
          , HH.button [ HE.onClick $ const $ ToggleFocus ] [ HH.text "Toggle Focus" ]
          ]
      -- 
      , HH.div [ HP.classes [ HH.ClassName "info" ] ]
          [ HH.div [ HP.classes [ HH.ClassName "info-item" ] ]
              [ HH.text $ "total steps: " <> show @Int (length state.history + length state.future) ]
          , HH.div [ HP.classes [ HH.ClassName "info-item" ] ]
              [ HH.text $ "focus: " <> if state.focusNew then "new" else "old" ]
          ]
      -- 
      , HH.div [ HP.classes [ HH.ClassName "view" ] ]
          [ case state.now of
              StaticExpr expr ->
                RS.renderExpr expr
                  # flip runReader
                      { renderA: HH.text
                      , mb_highlightPath: none
                      }
              UpdateExpr update ->
                RS.renderExpr (if state.focusNew then update.new else update.old)
                  # flip runReader
                      { renderA: HH.text
                      , mb_highlightPath: Just update.update.path
                      }
          ]
      -- 
      , HH.div [ HP.classes [ HH.ClassName "console" ] ]
          [ HH.div [ HP.classes [ HH.ClassName "console-title" ] ]
              [ HH.text "Console" ]
          , HH.div [ HP.classes [ HH.ClassName "console-menu" ] ]
              [ HH.button [ HE.onClick $ const ClearConsole ] [ HH.text "Clear" ]
              ]
          , HH.div [ HP.classes [ HH.ClassName "console-items" ] ] $
              state.messages # Array.fromFoldable # map \message ->
                HH.div [ HP.classes [ HH.ClassName "console-message" ] ]
                  [ message # HH.fromPlainHTML ]
          ]
      ]

addMessage :: Array PlainHTML -> AppState -> AppState
addMessage contents state = state { messages = state.messages # List.Cons (HH.span_ contents) }

setSystem
  :: forall r
   . System
  -> { messages :: List PlainHTML
     | r
     }
  -> AppState
setSystem system state =
  { messages: state.messages # List.Cons (HH.span_ [ HH.text $ "SetSystem " <> system.name ]) }
    # Record.insert (Proxy @"system") system
    # setExpr (getDefaultExprForSystemName system.name)

type PreAppState r = Record (PreAppStateR r)
type PreAppStateR r =
  ( system :: System
  , messages :: List PlainHTML
  | r
  )

setExpr
  :: forall r
   . Expr
  -> PreAppState r
  -> AppState
setExpr expr state =
  let
    (_ /\ _env') /\ trace = normalize expr
      # flip runReaderT (RS.newNormalizationCtx { system: state.system } {})
      # runExceptT
      # flip runStateT (RS.newNormalizationEnv { gas: 100 } {})
      # runWriterT
      # runIdentity
  in
    { system: state.system
    , history: none
    , now: case Array.uncons trace of
        Just { head: update } -> StaticExpr update.old
        _ -> StaticExpr expr
    , future: trace # map UpdateExpr # List.fromFoldable
    , messages: List.Cons
        ( HH.span_ $
            Array.intercalate [ HH.text " " ]
              [ [ HH.text "SetExpr" ]
              , [ RS.renderExpr expr
                    # flip runReader
                        ( RS.newRenderExprCtx
                            { mb_highlightPath: none
                            , renderA: HH.text
                            }
                        )
                ]
              ]
        )
        state.messages
    , focusNew: true
    }

getDefaultExprForSystemName :: String -> Expr
getDefaultExprForSystemName systemName = exampleExprs
  # Map.lookup systemName
  # fold
  # flip Array.index 0
  # fromMaybe' (\_ -> unsafeCrashWith "empty exampleExprs")
  # snd

examples
  :: Array
       ( String /\
           { rules :: Array Rule
           , exprs :: Array (String /\ Expr)
           }
       )
examples =
  [ Tuple "LC"
      { rules:
          [ case _ of
              "app" % [ "lam" % [ x % [], b ], a ] -> pure $ subst x a b
              _ -> Nothing
          ]
      , exprs:
          [ Tuple "ex1" $
              "app" % [ "lam" % [ "x" % [], "var" % [ "x" % [] ] ], "var" % [ "a" % [] ] ]
          ]
      }
  , Tuple "ACB"
      { rules:
          [ case _ of
              "A" % [] -> Just $ "B" % []
              _ -> Nothing
          , case _ of
              "B" % [] -> Just $ "C" % []
              _ -> Nothing
          , case _ of
              "C" % [] -> Just $ "A" % []
              _ -> Nothing
          ]
      , exprs:
          [ Tuple "ex1" $
              "A" % []
          ]
      }
  ]

exampleExprs :: Map String (Array (String /\ Expr))
exampleExprs = examples
  # map (\(systemName /\ x) -> (systemName /\ x.exprs))
  # Map.fromFoldable

exampleSystems :: Array System
exampleSystems = examples
  # map (\(systemName /\ x) -> { name: systemName, rules: x.rules })

subst :: String -> Expr -> Expr -> Expr
subst x a ("var" % [ y % [] ]) | x == y = a
subst x a (l % es) = l % (es # map (subst x a))
