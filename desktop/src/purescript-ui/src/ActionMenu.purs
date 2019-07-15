module ActionMenu where

import Prelude

import Data.Either.Nested (Either3)
import Data.Functor.Coproduct.Nested (Coproduct3(..))
import Data.Maybe (Maybe(..))
import FileLoad as FileLoad
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Core as HC
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import ResetButton as ResetButton
import SpeedAdjustment as SpeedAdjustment
import WhatUtils

-- Menu does not hold any state.
type State = Unit

-- This component doesn't currently take input from a parent component.
type Input = Unit

--This component isn't queryable right now
data Query a
    = QueryNone a

-- This component doesn't send any meaningful messages right now
data Message
    = MessageNone

type Slot = Either3 Unit Unit Unit

type ChildQuery = Coproduct3 ResetButton.Query SpeedAdjustment.Query FileLoad.Query


component :: forall m. H.Component HH.HTML Query Unit Void m
component =
  H.parentComponent
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }

initialState :: State
initialState = unit


render :: forall m. State -> H.ParentHTML Query ChildQuery Slot m
render state =
    HH.div
        [ HP.classes $ HC.ClassName <$> ["action-menu-component"]]
        [ HH.div
            [ HP.classes $ HC.ClassName <$> ["action-menu-item"]]
            [ HH.slot' resetSlot unit ResetButton.component unit (const Nothing) ]
        , HH.div
            [ HP.classes $ HC.ClassName <$> ["action-menu-item"]]
            [ HH.slot' speedSlot unit SpeedAdjustment.component unit (const Nothing) ]
        , HH.div
            [ classes ["action-menu-item"]]
            [ HH.slot' fileLoadSlot unit FileLoad.component unit (const Nothing)]
        ]

eval :: forall m. Query ~> H.ParentDSL State Query ChildQuery Slot Void m
eval q = 
    case q of
        QueryNone next -> pure next

resetSlot :: CP.ChildPath ResetButton.Query ChildQuery Unit Slot
resetSlot = CP.cp1

speedSlot :: CP.ChildPath SpeedAdjustment.Query ChildQuery Unit Slot
speedSlot = CP.cp2

fileLoadSlot :: CP.ChildPath FileLoad.Query ChildQuery Unit Slot
fileLoadSlot = CP.cp3