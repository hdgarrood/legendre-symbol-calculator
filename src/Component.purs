module Component where

import Prelude
import Control.MonadZero (guard)
import Data.ModularArithmetic.Primality (isPrime)
import Data.String as String
import Data.Array as Array
import Data.Foldable (for_)
import Data.List as List
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Int as Int
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import LegendreSymbols as LS
import Web.HTML.HTMLInputElement as HTMLInputElement
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window as Window
import Web.HTML (window)
import Web.DOM.NonElementParentNode (getElementById)

type State =
  -- The input is a tuple of the numerator and the denominator. The denominator
  -- should always be an odd prime.
  { input :: Tuple Int Int
  -- Count the number of reduction steps we should display; this is just the
  -- number of times the 'Next' button has been pressed.
  , reductions :: Int
  -- These fields just track what is in the UI, which may or may not be valid
  -- inputs.
  , numerator :: String
  , denominator :: String
  }

stateFromInput :: Tuple Int Int -> State
stateFromInput input =
  { input
  , reductions: 0
  , numerator: show (fst input)
  , denominator: show (snd input)
  }

isOddPrime :: Int -> Boolean
isOddPrime p = isPrime p && p > 2

-- | Attempt to update the state based on the contents of the two input fields.
-- | If the contents of the input fields do not constitute a valid input,
-- | return the state unmodified.
updateStateFromInputs :: State -> State
updateStateFromInputs state =
  let
    newInput = do
      a <- Int.fromString state.numerator
      p <- Int.fromString state.denominator
      guard (isOddPrime p)

      pure (Tuple a p)
  in
    maybe state stateFromInput newInput

data Query a
  = Advance a
  | ReplaceInput a
  | UpdateNumerator String a
  | UpdateDenominator String a

component :: H.Component HH.HTML Query Unit Void Aff
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
        HH.form [ HP.attr (H.AttrName "onsubmit") "return false" ]
          [ HH.span_ [ HH.text "Is" ]
          , HH.input
              [ HP.id_ numeratorId
              , HP.type_ HP.InputText
              , HP.title "Numerator"
              , HP.value (show (fst state.input))
              , HE.onValueChange (HE.input UpdateNumerator)
              ]
          , HH.span_ [ HH.text "a quadratic residue modulo" ]
          , HH.input
              [ HP.id_ denominatorId
              , HP.type_ HP.InputText
              , HP.title "Denominator"
              , HP.value (show (snd state.input))
              , HE.onValueChange (HE.input UpdateDenominator)
              ]
          , HH.span_ [ HH.text "?" ]
          , HH.input
              [ HP.type_ HP.InputSubmit
              , HP.value "Let's find out!"
              , HE.onClick (HE.input_ ReplaceInput)
              ]
          ]

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

  eval :: Query ~> H.ComponentDSL State Query Void Aff
  eval = case _ of
    Advance next -> do
      H.modify_ (\s -> s { reductions = s.reductions + 1 })
      pure next
    ReplaceInput next -> do
      H.modify_ updateStateFromInputs
      pure next
    UpdateNumerator new next -> do
      H.liftAff (validateNumerator new)
      H.modify_ (\s -> s { numerator = new })
      pure next
    UpdateDenominator new next -> do
      H.liftAff (validateDenominator new)
      H.modify_ (\s -> s { denominator = new })
      pure next

  numeratorId :: String
  numeratorId = "numerator"

  denominatorId :: String
  denominatorId = "denominator"

  -- Given an input element id and a validation function, validate that input
  -- element and update its custom validity status. The validation function
  -- should return an empty string on success, or an error message on failure.
  validateInputWith :: String -> (String -> String) -> String -> Aff Unit
  validateInputWith elId validate value = liftEffect do
    doc <- HTMLDocument.toNonElementParentNode <$> (window >>= Window.document)
    mel <- getElementById elId doc
    for_ (mel >>= HTMLInputElement.fromElement) \el ->
      HTMLInputElement.setCustomValidity (validate value) el

  validateNumerator :: String -> Aff Unit
  validateNumerator =
    validateInputWith numeratorId \value ->
      case Int.fromString value of
        Just _ -> ""
        Nothing -> "The numerator must be an integer."

  validateDenominator :: String -> Aff Unit
  validateDenominator =
    validateInputWith denominatorId \value ->
      case Int.fromString value of
        Nothing ->
          "The denominator must be an integer."
        Just p ->
          if isOddPrime p
            then ""
            else "The denominator must be an odd prime."
