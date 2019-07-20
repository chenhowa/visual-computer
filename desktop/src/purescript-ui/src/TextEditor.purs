module TextEditor where

import Effect.Console
import Prelude
import TextSelection
import WhatUtils

import Data.Function.Uncurried as F
import Data.Maybe (Maybe(..))
import Data.String (splitAt)
import Effect.Aff (Aff)
import Halogen (Action)
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML (a)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import TextEditor.TextDisplay as TextDisplay
import Web.Event.Event (Event, preventDefault)
import Web.HTML.HTMLInputElement (selectionStart)
import Web.HTML.HTMLOptionElement (index)
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.MouseEvent as ME



type State = Unit

type Input = String

data Query a 
    = NoneQuery a
    | HandleTextDisplay TextDisplay.Message a

data Message 
    = NoneMessage

type ChildQuery = TextDisplay.Query

data Slot = TextSlot
derive instance eqTextSlot :: Eq Slot
derive instance ordTextSlot :: Ord Slot



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
                [ classes [ "text-editor-component" ] ]     
                [ HH.slot TextSlot TextDisplay.component "" (HE.input HandleTextDisplay)]
        eval :: Query ~> H.ParentDSL State Query ChildQuery Slot Message Aff
        eval q = case q of 
            NoneQuery next -> pure next
            HandleTextDisplay m next -> do
                _ <- case m of 
                    TextDisplay.TextChanged -> H.query TextSlot $ H.action TextDisplay.SetSelection
                    TextDisplay.NonprintableKey -> H.query TextSlot $ H.action TextDisplay.SetSelection
                pure next