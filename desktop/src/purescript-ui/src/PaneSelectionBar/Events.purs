module PaneSelectionBar.Events 
    ( onContext
    , initMenu
    ) where

import Prelude
import Events.UI as UI
import Effect (Effect)
import Data.Function.Uncurried as UC


foreign import onContextImpl :: UC.Fn2 UI.Target (String -> Effect Unit) (Effect Unit)


onContext :: UI.Target -> (String -> Effect Unit) -> Effect Unit
onContext self fn = UC.runFn2 onContextImpl self fn

foreign import initializeMenu :: UC.Fn2 Boolean Boolean Unit

initMenu :: Boolean -> Boolean -> Unit 
initMenu text mem = UC.runFn2 initializeMenu text mem