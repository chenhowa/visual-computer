module App where

import Prelude
import WhatUtils

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
import ActionMenu as ActionMenu
import TextEditor as TextEditor

type State = Unit

type Input = Unit

data Query a 
    = NoneQuery a

data Message 
    = NoneMessage

type Slot = Either2 Unit Unit
type ChildQuery = Coproduct2 ActionMenu.Query TextEditor.Query

component :: H.Component HH.HTML Query Unit Void Aff
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
                [ classes ["app-component"] 
                ]
                [ HH.div
                    [ classes ["app-header"] ]
                    [ HH.slot' menuSlot unit ActionMenu.component unit (const Nothing)]
                , HH.div
                    [ classes ["app-body"]]
                    [ HH.slot' bodySlot unit TextEditor.component "" (const Nothing)]
                ]
        eval :: forall q. Query ~> H.ParentDSL State Query ChildQuery Slot Void q
        eval q = case q of 
            NoneQuery next -> pure next


menuSlot :: CP.ChildPath ActionMenu.Query ChildQuery Unit Slot
menuSlot = CP.cp1

bodySlot :: CP.ChildPath TextEditor.Query ChildQuery Unit Slot
bodySlot = CP.cp2


