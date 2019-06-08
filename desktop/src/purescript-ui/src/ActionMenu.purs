module ActionMenu where

import Prelude
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

-- Menu does not hold any state.
type State = Unit

-- This component doesn't currently take input from a parent component.
type Input = Unit

--This component isn't queryable right now
data Query a
    = QueryNone

-- This component doesn't send any meaningful messages right now
data Message
    = MessageNone