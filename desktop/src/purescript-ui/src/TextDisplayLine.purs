module TextDisplayLine where

import Effect.Console
import Prelude

import Data.Function.Uncurried as F
import Data.Maybe (Maybe(..))
import Data.String (length)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML (a)
import Halogen.HTML as HH
import Halogen.HTML.Core as HC
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import TextSelection (setSelection, startIndex, endIndex)
import WhatUtils (classes, constructNewText, isPrintable, backspaceText, deleteText)
import Web.Event.Event (Event, preventDefault)
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.MouseEvent as ME

type State = Line
type Line = String
type Input = String

data Query a 
    = ResetLine String a
    | HandleInput Input a

type Message = Void

{-data Message 
    = NoneMessage-}

component :: H.Component HH.HTML Query Input Message Aff 
component =
    H.component 
        { initialState: initialize
        , render
        , eval
        , receiver: HE.input HandleInput
        }
    where 
        initialize :: Input -> State
        initialize input = input

        render :: State -> H.ComponentHTML Query
        render state =
            HH.div
                [ classes [ "text-display-line-component" ]
                ]
                [ HH.text state
                ]
        eval :: Query ~> H.ComponentDSL State Query Message Aff
        eval q = case q of 
            ResetLine str next -> do
                H.put str 
                pure next
            HandleInput str next -> do
                eval $ ResetLine str next 