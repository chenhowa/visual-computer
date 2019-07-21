module PaneSelectionBar 
    ( component
    , State 
    , Input 
    , Message 
    , Query
    , MyMessage(..)
    , MyQuery(..)
    , SelectedPanes
    ) where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core as HC
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Effect.Aff (Aff)
import Effect.Console (log)

import WhatUtils as U
import Events.UI as EUI
import PaneSelectionBar.Events as PSBE



type State = SelectedPanes

type SelectedPanes = 
    { textEditor :: Boolean 
    , memoryEditor :: Boolean
    }

type Input = Maybe SelectedPanes

type Query = MyQuery

data MyQuery a 
    = NoneQuery a
    | Initialize a
    | HandleContext String (H.SubscribeStatus -> a)
    | HandleInput Input a

type Message = MyMessage

data MyMessage 
    = ShowTextEditor Boolean
    | ShowMemoryEditor Boolean

component :: H.Component HH.HTML Query Input Message Aff
component = 
    H.lifecycleComponent 
        { initialState: initialize
        , render
        , eval
        , receiver: HE.input HandleInput
        , initializer: Just $ H.action Initialize
        , finalizer: Nothing
        }
    where
        initialize :: Input -> State 
        initialize i = case i of 
            Just state -> state
            Nothing -> 
                { textEditor: false
                , memoryEditor: false
                }                

        render :: State -> H.ComponentHTML Query
        render state = do
            HH.footer 
                [ U.classes [ componentClass ]

                ] 
                [ HH.div 
                    [ U.classes [ ] ]
                    [ HH.text "HI PSB"
                    ]
                ]
            
        eval :: Query ~> H.ComponentDSL State Query Message Aff
        eval q = case q of 
            NoneQuery next -> pure next
            Initialize next -> do
                state <- H.get
                let _ = PSBE.initMenu state.textEditor state.memoryEditor
                case EUI.target componentClass of 
                    Nothing -> pure unit 
                    Just t -> H.subscribe $ H.eventSource (PSBE.onContext t) (Just <<< H.request <<< HandleContext)
                pure next
            HandleContext str reply -> do
                H.liftEffect $ log $ "handling context menu request for " <> str
                state <- H.get 
                case str of 
                    "textEditor" -> H.raise $ ShowTextEditor (not state.textEditor)
                    "memoryEditor" -> H.raise $ ShowMemoryEditor (not state.textEditor)
                    _ -> pure unit
                pure (reply H.Listening)
            HandleInput input next -> do
                case input of
                    Nothing -> pure unit 
                    Just panes -> do 
                        H.put panes
                        pure unit
                pure next

componentClass :: String 
componentClass = "pane-selection-bar-component"