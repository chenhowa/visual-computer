module AppBody 
    ( component
    , State 
    , Input 
    , Query
    , Message
    , MyQuery 
    , MyMessage
    ) where 

import Prelude
import WhatUtils as U

import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML (a)
import Halogen.HTML as HH
import Halogen.HTML.Core as HC
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Component.ChildPath as CP

import Effect.Aff (Aff)


import Data.Either.Nested
import Data.Functor.Coproduct.Nested

import TextEditor as TextEditor
import MemoryEditor as MemoryEditor

type State = Unit

type Input = Unit

type Query = MyQuery

data MyQuery a 
    = NoneQuery a

type Message = MyMessage

data MyMessage 
    = NoneMessage

type Slot = Either2 Unit Unit
type ChildQuery = Coproduct2 TextEditor.Query MemoryEditor.Query

component :: H.Component HH.HTML Query Input Message Aff
component = 
    H.parentComponent 
        { initialState: const initialState
        , render
        , eval
        , receiver: const Nothing
        }
    where
        initialState :: State 
        initialState = unit

        render :: State -> H.ParentHTML Query ChildQuery Slot Aff
        render state = 
            HH.div
                [ U.classes ["app-body"]
                ]
                [ HH.slot' textSlot unit TextEditor.component "" (const Nothing)
                , HH.slot' memorySlot unit MemoryEditor.component Nothing (const Nothing)
                ]
        eval :: forall q. Query ~> H.ParentDSL State Query ChildQuery Slot Message q
        eval q = case q of 
            NoneQuery next -> pure next

textSlot :: CP.ChildPath TextEditor.Query ChildQuery Unit Slot
textSlot = CP.cp1

memorySlot :: CP.ChildPath MemoryEditor.Query ChildQuery Unit Slot
memorySlot = CP.cp2