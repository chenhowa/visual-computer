module SpeedAdjustment where
  
import Prelude

import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Core as HC

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

component :: forall m. H.Component HH.HTML Query Input Message m
component =
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
                title = "Adjust Speed"
            in 
                HH.div
                    [ HP.classes $ HC.ClassName <$> ["speed-adjustment-component"]]
                    [ renderSpeedSetting state
                    , renderSpeedSetting state
                    , renderSpeedSetting state
                    ]

        eval :: forall a. Query ~> H.ComponentDSL State Query Message a
        eval q = case q of
            SetSlow next -> pure next
            SetMedium next -> pure next 
            SetFast next -> pure next

renderSpeedSetting :: State -> H.ComponentHTML Query
renderSpeedSetting name = 
    HH.div 
        [ HP.classes $ HC.ClassName <$> [ "speed-setting" ]]
        []