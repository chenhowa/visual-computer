module AppBody 
    ( component
    , State 
    , Input 
    , Query
    , Message
    , MyQuery(..)
    , MyMessage(..)
    , MyState(..)
    , Layout(..)
    ) where 

import Data.Either.Nested
import Data.Functor.Coproduct.Nested
import Prelude

import AppBody.TabLayout as TabLayout
import AppBody.ColumnLayout as ColumnLayout
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Console (log)
import Events.UI as UI
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML (a)
import Halogen.HTML as HH
import Halogen.HTML.Core as HC
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import PaneSelectionBar as PSB
import Web.Event.Event as E
import Web.UIEvent.MouseEvent as ME
import WhatUtils as U

type State = MyState 

type MyState =
    { showTextEditor :: Boolean
    , showMemoryEditor :: Boolean
    , layout :: Layout
    }

type Input = Maybe State

type Query = MyQuery

data MyQuery a 
    = NoneQuery a
    | HandlePSB PSB.Message a

data Layout 
    = Tab
    | Column

type Message = MyMessage

data MyMessage 
    = NoneMessage

type Slot = Either3 Unit Unit Unit
type ChildQuery = Coproduct3 TabLayout.Query ColumnLayout.Query PSB.Query

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
                , layout: Column
                }

        render :: State -> H.ParentHTML Query ChildQuery Slot Aff
        render state = 
            HH.div
                [ U.classes ["app-body-component" ]
                ]
                [ HH.div 
                    [ U.classes ["app-body-content"]]
                    [ layout state.layout
                    , HH.slot' psbSlot unit PSB.component (Just psbInput) (HE.input HandlePSB)
                    ]
                ]
            where
                psbInput = 
                    { textEditor: state.showTextEditor, memoryEditor: state.showMemoryEditor }
                tabLayoutInput = 
                    { showTextEditor: state.showTextEditor, showMemoryEditor: state.showMemoryEditor }
                colLayoutInput = 
                    { showTextEditor: state.showTextEditor, showMemoryEditor: state.showMemoryEditor }
                layout l = case l of 
                    Tab -> HH.slot' tabLayoutSlot unit TabLayout.component (Just tabLayoutInput) (const Nothing)
                    Column -> HH.slot' colLayoutSlot unit ColumnLayout.component (Just colLayoutInput) (const Nothing)

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
                    PSB.UseTabLayout -> do 
                        H.liftEffect $ log "useTabLayout"
                        H.put $ state { layout = Tab }
                    PSB.UseColumnLayout -> do
                        H.liftEffect $ log "useColumnLayout"
                        H.put $ state { layout = Column }
                pure next

tabLayoutSlot :: CP.ChildPath TabLayout.Query ChildQuery Unit Slot
tabLayoutSlot = CP.cp1

colLayoutSlot :: CP.ChildPath ColumnLayout.Query ChildQuery Unit Slot
colLayoutSlot = CP.cp2

psbSlot :: CP.ChildPath PSB.Query ChildQuery Unit Slot
psbSlot = CP.cp3