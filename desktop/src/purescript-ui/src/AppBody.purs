module AppBody 
    ( component
    , State 
    , Input 
    , Query
    , Message
    , MyQuery(..)
    , MyMessage(..)
    , MyState(..)
    , Active(..)
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


import MemoryEditor as MemoryEditor
import PaneSelectionBar as PSB
import TextEditor as TextEditor
import WhatUtils as U
import Events.UI as UI

type State = MyState 

type MyState =
    { showTextEditor :: Boolean
    , showMemoryEditor :: Boolean
    , active :: Active
    }

data Active
    = Text
    | Memory
    | None

derive instance eqActive :: Eq Active

type Input = Maybe State

type Query = MyQuery

data MyQuery a 
    = NoneQuery a
    | HandlePSB PSB.Message a
    | TabClick Active a

type Message = MyMessage

data MyMessage 
    = NoneMessage

type Slot = Either3 Unit Unit Unit
type ChildQuery = Coproduct3 TextEditor.Query MemoryEditor.Query PSB.Query

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
                , active: Text
                }

        render :: State -> H.ParentHTML Query ChildQuery Slot Aff
        render state = 
            let psbInput = 
                    { textEditor: state.showTextEditor, memoryEditor: state.showMemoryEditor }
                programTitle = "Program"
                memoryTitle = "Memory"
            in 
                HH.div
                    [ U.classes ["app-body-component" ]
                    ]
                    [ HH.ul 
                        [ U.classes ["app-body-tabs"] ]
                        [ tab programTitle Text state.active state.showTextEditor 
                        , tab memoryTitle Memory state.active state.showMemoryEditor
                        ]
                    , HH.div 
                        [ U.classes ["app-body-tab-content"] ]
                        [ textEditPane state.active state.showTextEditor
                        , memoryEditPane state.active state.showMemoryEditor
                        , HH.slot' psbSlot unit PSB.component (Just psbInput) (HE.input HandlePSB)
                        ]
                    ]
        eval :: Query ~> H.ParentDSL State Query ChildQuery Slot Message Aff
        eval q = case q of 
            NoneQuery next -> pure next
            HandlePSB mess next -> do
                state <- H.get
                case mess of 
                    PSB.ShowTextEditor b -> do
                        H.put $ activeIfShownState state { showTextEditor = b } 
                        pure unit  
                    PSB.ShowMemoryEditor b -> do
                        H.put $ activeIfShownState state { showMemoryEditor = b }
                        pure unit
                pure next
            TabClick active next -> do 
                state <- H.get 
                H.put state 
                    { active = active
                    }
                pure next

textSlot :: CP.ChildPath TextEditor.Query ChildQuery Unit Slot
textSlot = CP.cp1

memorySlot :: CP.ChildPath MemoryEditor.Query ChildQuery Unit Slot
memorySlot = CP.cp2

psbSlot :: CP.ChildPath PSB.Query ChildQuery Unit Slot
psbSlot = CP.cp3

tab :: String -> Active -> Active -> Boolean -> H.ParentHTML Query ChildQuery Slot Aff
tab str c current doShow = 
    HH.li 
        [ U.classes [ "app-body-tab-item"]
        , HE.onClick $ HE.input_ (TabClick $ c)
        ]
        [ HH.a 
            [ U.classes $ ["app-body-tab-item-link"] <> active <> show
            , HP.href "#"
            ]
            [ HH.text str ]
        ]
    where 
        active = if c == current then ["active", "in" ] else []
        show = if doShow then [] else [ "hidden" ]

textEditPane :: Active -> Boolean -> H.ParentHTML Query ChildQuery Slot Aff
textEditPane actual doShow = 
    pane Text actual [ HH.slot' textSlot unit TextEditor.component "" (const Nothing) ] doShow
    where 
        pane c current children s = 
            HH.div 
                [ U.classes $ ["app-body-pane"] <> active <> show
                ]
                children
            where 
                active = if c == current && s then ["active", "in"] else []
                show = if s then ["hidden"] else []

memoryEditPane :: Active -> Boolean -> H.ParentHTML Query ChildQuery Slot Aff
memoryEditPane actual doShow = 
    pane Memory actual [ HH.slot' memorySlot unit MemoryEditor.component Nothing (const Nothing) ] doShow
    where 
        pane c current children s = 
            HH.div 
                [ U.classes $ ["app-body-pane"] <> active <> show
                ]
                children
            where
                active = if c == current && s then ["active", "in"] else []
                show = if s then ["hidden"] else []

activeIfShownState :: State -> State 
activeIfShownState state = 
    case state.active of 
        Text -> 
            if not state.showTextEditor then activeIfShownState state { active = Memory } 
            else state 
        Memory -> if not state.showMemoryEditor then activeIfShownState state { active = None } else state 
        None -> 
            if state.showTextEditor 
            then state { active = Text } 
            else 
                if state.showMemoryEditor
                then state { active = Memory }
                else state