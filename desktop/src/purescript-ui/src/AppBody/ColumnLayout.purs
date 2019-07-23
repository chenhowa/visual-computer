module AppBody.ColumnLayout 
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

import AppBody.ColumnLayout.Sizing as Sizing
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
import MemoryEditor as MemoryEditor
import TextEditor as TextEditor
import Web.Event.Event as E
import Web.UIEvent.MouseEvent as ME
import WhatUtils as U


type State = MyState 

type MyState =
    { showTextEditor :: Boolean
    , showMemoryEditor :: Boolean
    }

type Input = Maybe { showTextEditor :: Boolean, showMemoryEditor :: Boolean }

type Query = MyQuery

data MyQuery a 
    = Initialize a
    | HandleInput Input a

type Message = MyMessage

data MyMessage 
    = NoneMessage

type Slot = Either2 Unit Unit
type ChildQuery = Coproduct2 TextEditor.Query MemoryEditor.Query

component :: H.Component HH.HTML Query Input Message Aff
component = 
    H.lifecycleParentComponent 
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
            Just s ->
                { showTextEditor: s.showTextEditor
                , showMemoryEditor: s.showMemoryEditor
                }
            Nothing -> 
                { showTextEditor: true
                , showMemoryEditor: true
                }

        render :: State -> H.ParentHTML Query ChildQuery Slot Aff
        render state = 
            HH.div
                [ U.classes [ "column-layout-component" ]
                ]
                [ textColumn state.showTextEditor
                , memoryColumn state.showMemoryEditor
                ]
        eval :: Query ~> H.ParentDSL State Query ChildQuery Slot Message Aff
        eval q = case q of 
            Initialize next -> do
                let _ = Sizing.setResizable unit
                pure next
            HandleInput input next -> do 
                state <- H.get
                case input of 
                    Nothing -> pure unit 
                    Just i -> 
                        H.put $ state
                                    { showTextEditor = i.showTextEditor 
                                    , showMemoryEditor = i.showMemoryEditor
                                    }
                pure next

textColumn :: Boolean -> H.ParentHTML Query ChildQuery Slot Aff
textColumn show = 
    HH.div 
        [ U.classes $ [ "cl-column"] <> U.hiddenClasses show
        , HP.id_ "one"
        ]
        [ HH.slot' textSlot unit TextEditor.component "" (const Nothing)
        ]

memoryColumn :: Boolean -> H.ParentHTML Query ChildQuery Slot Aff
memoryColumn show = 
    HH.div 
        [ U.classes $ [ "cl-column" ] <> U.hiddenClasses show
        , HP.id_ "two"
        ]
        [ HH.slot' memorySlot unit MemoryEditor.component Nothing (const Nothing)
        ]


textSlot :: CP.ChildPath TextEditor.Query ChildQuery Unit Slot
textSlot = CP.cp1

memorySlot :: CP.ChildPath MemoryEditor.Query ChildQuery Unit Slot
memorySlot = CP.cp2