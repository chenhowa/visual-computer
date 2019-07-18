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
import TextSelection as TS
import WhatUtils (classes, constructNewText, isPrintable, backspaceText, deleteText)
import Web.Event.Event (Event, preventDefault)
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.MouseEvent as ME

type State = Line
type Line =
    { text :: String
    , number :: Int
    }
type Input = Line

data Query a 
    = ResetText String a
    | ResetNumber Int a
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
                [ HH.text (if state.number == 1 then state.text else "\r" <> state.text)
                ]
        eval :: Query ~> H.ComponentDSL State Query Message Aff
        eval q = case q of 
            ResetText str next -> do
                state <- H.get
                H.put $ state { text = str }
                pure next
            ResetNumber num next -> do
                state <- H.get 
                H.put $ state { number = num }
                pure next
            HandleInput input next -> do
                _ <- eval $ ResetText input.text next
                eval $ ResetNumber input.number next