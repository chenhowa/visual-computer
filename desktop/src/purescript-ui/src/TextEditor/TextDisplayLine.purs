module TextEditor.TextDisplayLine where

import Effect.Console
import Prelude

import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import WhatUtils (classes)

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
                [ --HH.text (if state.number == 1 then state.text else "\r" <> state.text)
                  HH.text $ "\r" <> state.text
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