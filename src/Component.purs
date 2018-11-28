module Component where

import Prelude
import Control.MonadZero (guard)
import Control.Monad.Error.Class (throwError)
import Data.ModularArithmetic.Primality (isPrime)
import Data.Foldable (fold)
import Data.String as String
import Data.Array as Array
import Data.List as List
import Data.Maybe (Maybe(..), maybe)
import Data.Either (Either(..), either, note)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Int as Int
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import LegendreSymbols as LS

type State =
  { input :: Tuple Int Int
  , reductions :: Int
  , numerator :: String
  , denominator :: String
  , validationError :: Maybe String
  }

stateFromInput :: Tuple Int Int -> State
stateFromInput input =
  { input
  , reductions: 0
  , numerator: show (fst input)
  , denominator: show (snd input)
  , validationError: Nothing
  }

updateInput :: State -> State
updateInput state =
  let
    newInput = do
      a <- note "Numerator must be an integer"
             (Int.fromString state.numerator)
      p <- note "Denominator must be an integer"
             (Int.fromString state.denominator)

      when (not (isPrime p) || p == 2)
        (throwError "Denominator must be an odd prime")

      pure (Tuple a p)
  in
    either (\err -> state { validationError = Just err })
           stateFromInput
           newInput

data Query a
  = Advance a
  | ReplaceInput a
  | UpdateNumerator String a
  | UpdateDenominator String a

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

      reductions :: LS.Reductions
      reductions =
        LS.reduceN state.reductions (LS.single state.input)

      isComplete =
        reductions.steps
        # List.last
        # map (LS.isFullyReduced <<< snd)
        # (_ == Just true)

      inputForm =
        HH.div_ $
          [ HH.input
              [ HP.type_ HP.InputText
              , HP.title "Numerator"
              , HP.value (show (fst state.input))
              , HE.onValueChange (HE.input UpdateNumerator)
              ]
          , HH.input
              [ HP.type_ HP.InputText
              , HP.title "Denominator"
              , HP.value (show (snd state.input))
              , HE.onValueChange (HE.input UpdateDenominator)
              ]
          , HH.button
              [ HP.title "Go"
              , HE.onClick (HE.input_ ReplaceInput)
              ]
              [ HH.text "Go" ]
          ] <> Array.fromFoldable (map renderValidationError state.validationError)

      renderValidationError msg =
        HH.p_ [ HH.text msg ]

      workingPlain =
        reductions.steps
        # map (\(Tuple rule e) -> LS.exprToString e <> "     (by " <> String.drop 4 (show rule) <> ")")
        # List.Cons (LS.exprToString reductions.initial)
        # Array.fromFoldable
        # String.joinWith "\n  = "

      displayPanel =
        HH.pre_ [ HH.text workingPlain ]

      nextButton =
        HH.button
          [ HP.title label
          , HP.enabled (not isComplete)
          , HE.onClick (HE.input_ Advance)
          ]
          [ HH.text label ]
    in
      HH.div_ [ inputForm, displayPanel, nextButton ]

  eval :: Query ~> H.ComponentDSL State Query Void m
  eval = case _ of
    Advance next -> do
      H.modify_ (\s -> s { reductions = s.reductions + 1 })
      pure next
    ReplaceInput next -> do
      H.modify_ updateInput
      pure next
    UpdateNumerator new next -> do
      H.modify_ (\s -> s { numerator = new })
      pure next
    UpdateDenominator new next -> do
      H.modify_ (\s -> s { denominator = new })
      pure next
