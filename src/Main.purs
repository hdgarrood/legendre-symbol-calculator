module Main where

import Prelude
import Data.Maybe (Maybe(..))
import Web.DOM.ParentNode (QuerySelector(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (error)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Component (component)

selector :: QuerySelector
selector = QuerySelector "#app"

main :: Effect Unit
main = HA.runHalogenAff do
  HA.awaitLoad
  mel <- HA.selectElement selector
  case mel of
    Just el ->
      void $ runUI component unit el
    Nothing ->
      liftEffect (error "Unable to find #app element")
