module PaneSelectionBar.Events 
    ( onContext

    ) where

import Prelude
import Events.UI as UI
import Effect (Effect)
import Data.Function.Uncurried as UC


foreign import onContextImpl :: UC.Fn2 UI.Target (String -> Effect Unit) (Effect Unit)


onContext :: UI.Target -> (String -> Effect Unit) -> Effect Unit
onContext self fn = UC.runFn2 onContextImpl self fn