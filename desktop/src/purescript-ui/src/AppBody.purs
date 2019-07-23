module AppBody 
    ( component
    , State 
    , Input 
    , Query
    , Message
    , MyQuery(..)
    , MyMessage(..)
    , MyState(..)
    ) where 

import Data.Either.Nested
import Data.Functor.Coproduct.Nested
import Prelude
import Effect.Console (log)

import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML (a)
import Halogen.HTML as HH
import Halogen.HTML.Core as HC
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.UIEvent.MouseEvent as ME
import Web.Event.Event as E


import PaneSelectionBar as PSB
import AppBody.TabLayout as TabLayout
import WhatUtils as U
import Events.UI as UI

type State = MyState 

type MyState =
    { showTextEditor :: Boolean
    , showMemoryEditor :: Boolean
    }

type Input = Maybe State

type Query = MyQuery

data MyQuery a 
    = NoneQuery a
    | HandlePSB PSB.Message a

type Message = MyMessage

data MyMessage 
    = NoneMessage

type Slot = Either2 Unit Unit
type ChildQuery = Coproduct2 TabLayout.Query PSB.Query

component :: H.Component HH.HTML Query Input Message Aff
component = 
    H.parentComponent 
        { initialState: initialize
        , render
        , eval
        , receiver: const Nothing
        }
    where
        initialize :: Input -> State 
        initialize i = case i of 
            Just s -> s 
            Nothing -> 
                { showTextEditor: true
                , showMemoryEditor: true
                }

        render :: State -> H.ParentHTML Query ChildQuery Slot Aff
        render state = 
            let psbInput = 
                    { textEditor: state.showTextEditor, memoryEditor: state.showMemoryEditor }
                tablayoutInput = 
                    { showTextEditor: state.showTextEditor, showMemoryEditor: state.showMemoryEditor }
            in 
                HH.div
                    [ U.classes ["app-body-component" ]
                    ]
                    [ HH.slot' tabLayoutSlot unit TabLayout.component (Just tablayoutInput) (const Nothing)
                    , HH.slot' psbSlot unit PSB.component (Just psbInput) (HE.input HandlePSB)
                    ]
        eval :: Query ~> H.ParentDSL State Query ChildQuery Slot Message Aff
        eval q = case q of 
            NoneQuery next -> pure next
            HandlePSB mess next -> do
                state <- H.get
                case mess of 
                    PSB.ShowTextEditor b -> do
                        H.liftEffect $ log "showTextEditor"
                        H.put $ state { showTextEditor = b } 
                        pure unit  
                    PSB.ShowMemoryEditor b -> do
                        H.liftEffect $ log "showMemoryEditor"
                        H.put $ state { showMemoryEditor = b }
                        pure unit
                pure next

tabLayoutSlot :: CP.ChildPath TabLayout.Query ChildQuery Unit Slot
tabLayoutSlot = CP.cp1

psbSlot :: CP.ChildPath PSB.Query ChildQuery Unit Slot
psbSlot = CP.cp2