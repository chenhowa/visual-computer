module FileLoad where 

import Prelude
import WhatUtils

import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import RemoteElectron as RE


-- Possibly file load is unitless. Or maybe it containst the file that is loaded
type State = FileName 

type FileName = String

type Input = Unit

data Query a = 
    Click a

data Message = 
    NoneMessage

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
        initialState = "File1"

        render :: State -> H.ComponentHTML Query
        render state = 
            HH.div
                [ classes ["file-load-component"]]
                [ HH.button
                    [ classes ["file-load-button"]
                    , HE.onClick (HE.input_ Click)
                    ]
                    [ HH.text state
                    ]
                ]

        eval :: forall a. Query ~> H.ComponentDSL State Query Message a
        eval q = case q of
            Click next -> do 
                let file = RE.openFileDialog unit
                H.put $ file   
                pure next
