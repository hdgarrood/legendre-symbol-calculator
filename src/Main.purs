module Main where

import Prelude
import Data.Maybe (Maybe(..))
import DOM.Node.ParentNode (QuerySelector(..))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (error)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Component (component)

selector :: QuerySelector
selector = QuerySelector "#app"

main :: Eff _ Unit
main = HA.runHalogenAff do
  HA.awaitLoad
  mel <- HA.selectElement selector
  case mel of
    Just el ->
      void $ runUI component unit el
    Nothing ->
      liftEff (error "Unable to find #app element")
