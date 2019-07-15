module Main where

import Prelude
import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import App as App
import Web.DOM.ParentNode (QuerySelector(..))
import Data.Foldable (traverse_)

main :: Effect Unit
main = HA.runHalogenAff do
  HA.awaitLoad
  (HA.selectElement $ QuerySelector "#app") >>= 
    traverse_ \container -> do
      io <- runUI App.component unit container
      pure unit
  pure unit
