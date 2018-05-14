module Component where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type State =
  { input :: Tuple Int Int
  , reductions :: Int
  }

stateFromInput :: Tuple Int Int -> State
stateFromInput input = { input, reductions: 0 }

data Query a
  = Advance a
  | ReplaceInput (Tuple Int Int) a

component :: forall m. H.Component HH.HTML Query Unit Void m
component =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = stateFromInput (Tuple 30 47)

  render :: State -> H.ComponentHTML Query
  render state =
    let
      label = "Next"
    in
      HH.button
        [ HP.title label
        , HE.onClick (HE.input_ Advance)
        ]
        [ HH.text label ]

  eval :: Query ~> H.ComponentDSL State Query Void m
  eval = case _ of
    Advance next -> do
      H.modify (\s -> s { reductions = s.reductions + 1 })
      pure next
    ReplaceInput newInput next -> do
      H.put (stateFromInput newInput)
      pure next
