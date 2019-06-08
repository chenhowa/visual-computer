module Main where

import Prelude
import Effect (Effect)
import Halogen.Aff as HA
import Data.Maybe (Maybe(..))
import Halogen.VDom.Driver (runUI)
import ResetButton as ResetButton
import Effect.Console (log)
import Web.DOM.ParentNode (QuerySelector(..))
import Data.Foldable (traverse_)

main :: Effect Unit
main = HA.runHalogenAff do
  HA.awaitLoad
  (HA.selectElement $ QuerySelector "#app") >>= 
    traverse_ \container -> do
      io <- runUI ResetButton.ui unit container
      pure unit
  pure unit
