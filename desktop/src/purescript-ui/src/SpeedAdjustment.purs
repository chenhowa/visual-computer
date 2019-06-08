module SpeedAdjustment where
  
import Prelude
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

-- What speed is it set at?
type State = Speed 

data Speed
    = Slow
    | Medium
    | Fast

instance showSpeed :: Show Speed where 
    show speed = case speed of 
        Slow -> "Slow"
        Medium -> "Medium"
        Fast -> "Fast"

-- This component currently doesn't take input
type Input = Unit

data Query a
    = SetSlow a
    | SetMedium a
    | SetFast a

data Message 
    = SpeedSet Speed

ui :: forall m. H.Component HH.HTML Query Input Message m
ui =
    H.component 
        { initialState: const initialState
        , render
        , eval
        , receiver: const Nothing

        }
    where 
        initialState :: State
        initialState = Medium

        render :: State -> H.ComponentHTML Query
        render state = 
            let 
                label = "Adjust" <> " " <> show state
            in 
                HH.button
                    [ HP.title label
                    , HE.onClick (HE.input_ SetSlow ) 
                    ]
                    [ HH.text label ]

        eval :: forall m. Query ~> H.ComponentDSL State Query Message m
        eval q = case q of
            SetSlow next -> pure next
            SetMedium next -> pure next 
            SetFast next -> pure next