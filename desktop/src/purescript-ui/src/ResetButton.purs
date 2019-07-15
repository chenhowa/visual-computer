module ResetButton where

import Prelude
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Core as HC


-- Is the button clicked or not?
type State = Int

-- This component doesn't currently take input from a parent component.
type Input = Unit

data Query a
    = Click a

data Message
    = Clicked Int

component :: forall m. H.Component HH.HTML Query Input Message m
component =
    H.component 
        { initialState: const initialState
        , render
        , eval
        , receiver: const Nothing

        }

initialState :: State
initialState = 0

render :: State -> H.ComponentHTML Query
render state = 
    let 
        label = "RESET" <> " " <> show state
    in 
        HH.button
            [ HP.title label
            , HE.onClick (HE.input_ Click ) 
            , HP.classes $ HC.ClassName <$> ["reset-button-component"]
            ]
            [ HH.text label ]


eval :: forall m. Query ~> H.ComponentDSL State Query Message m
eval q = case q of
    Click next -> do
        state <- H.get
        let nextState = state + 1
        H.put nextState
        H.raise $ Clicked nextState
        pure next