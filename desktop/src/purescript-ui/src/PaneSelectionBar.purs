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

import WhatUtils as U



type State = SelectedPanes

type SelectedPanes = 
    { textEditor :: Boolean 
    , memoryEditor :: Boolean
    }

type Input = Maybe SelectedPanes

type Query = MyQuery

data MyQuery a 
    = NoneQuery a

type Message = MyMessage

data MyMessage 
    = NoneMessage

component :: H.Component HH.HTML Query Input Message Aff
component = 
    H.component 
        { initialState: initialize
        , render
        , eval
        , receiver: const Nothing
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
        render state = 
            HH.div 
                [ U.classes [ "pane-selection-bar-component" ]

                ] 
                [ HH.div 
                    [ ]
                    [ HH.text "HI PSB"
                    ]
                ]
            
        eval :: Query ~> H.ComponentDSL State Query Message Aff
        eval q = case q of 
            NoneQuery next -> pure next