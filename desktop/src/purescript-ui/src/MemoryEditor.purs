module MemoryEditor
    ( component
    , State
    , Message 
    , Input
    , Query
    , MyQuery 
    , MyMessage
    , Datum
    ) where

import MemoryEditor.Mode as Mode
import Prelude

import Data.Array as Array
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Tuple (Tuple(..))
import Data.Tuple as Tuple
import Effect.Aff (Aff)
import Effect.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core as HC
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import WhatUtils as U

type State =
    { data :: Array Datum
    , refreshCount :: Int 
    , mode :: Mode.Mode
    }

type Datum = Int

type Input = Maybe State

type Query = MyQuery

data MyQuery a 
    = RowDataUpdate Int String a
    | ModeChange Mode.Mode a

type Message = MyMessage

data MyMessage 
    = NoneMessage

component :: H.Component HH.HTML Query Input Message Aff
component = 
    H.component 
        { initialState: initialize
        , render
        , eval
        , receiver: const Nothing
        }
    where
        initialize :: Input -> State 
        initialize i = case i of 
            Just state -> state
            Nothing -> 
                { data: [ 5, 6, 17]
                , refreshCount: 0
                , mode: Mode.Decimal
                }                

        render :: State -> H.ComponentHTML Query
        render state = 
            HH.div
                [ U.classes ["memory-editor-component"]
                ]
                [ HH.div 
                    [ U.classes ["memory-editor-table"] ]
                    ( header <> rows
                    )
                ]
            where 
                header = 
                    [ HH.div
                        [ U.classes ["met-row"] ]
                        [ HH.div
                            [ U.classes ["met-cell", "met-cell-memory-number-header"] ]
                            [ HH.text "Address" ]
                        , HH.div
                            [ U.classes ["met-cell", "met-cell-memory-data-header"] ]
                            [ HH.text "Value" ]
                        , HH.div
                            [ U.classes ["met-options"] ]
                            [ HH.select 
                                [ HE.onValueChange $ handleModeChange unit

                                ]
                                [ HH.option_ [ HH.text "Decimal" ]
                                , HH.option_ [ HH.text "Binary" ]
                                , HH.option_ [ HH.text "Hexadecimal" ]
                                ]

                            ]
                        ] 
                    ]
                rows = 
                    let 
                        memoryData = state.data
                        memoryNumbers = Array.range 1 $ Array.length state.data
                        memoryRow = Array.zip memoryNumbers memoryData
                    in renderRow <$> memoryRow
                renderRow tup = 
                    HH.div 
                        [ U.classes ["met-row"] 
                        ] 
                        [ HH.div 
                            [ U.classes ["met-cell", "met-cell-memory-number"] ]
                            [ HH.text $ show $ Tuple.fst tup ]
                        , HH.div
                            [ U.classes ["met-cell", "met-cell-memory-data"] ]
                            [ HH.input 
                                [ U.classes ["met-input"]
                                , HP.value $ (String.fromCodePointArray $ 
                                    Array.concat $ Array.replicate state.refreshCount $ 
                                    String.toCodePointArray "\r") <> (Mode.intToString state.mode $ Tuple.snd tup)
                                , HE.onValueChange $ handleInputChange (Tuple.fst tup) unit
                                , HH.attr (HH.AttrName "refreshCount") (show state.refreshCount)
                                ]
                            ]
                        ]
        eval :: Query ~> H.ComponentDSL State Query Message Aff
        eval q = case q of 
            RowDataUpdate row datum next -> do
                state <- H.get
                let newState = rowDataUpdateState state row datum
                H.put newState
                H.liftEffect $ log (show newState)
                pure next
            ModeChange mode next -> do
                H.liftEffect $ log "mode changed!"
                state <- H.get
                H.put state { mode = mode }
                pure next

handleInputChange :: forall a. Int -> a -> String -> Maybe (Query a)
handleInputChange i a s = Just $ RowDataUpdate i s a

handleModeChange :: forall a. a -> String -> Maybe (Query a)
handleModeChange a s = 
    let mode = case s of 
            "Decimal" -> Mode.Decimal
            "Binary" -> Mode.Binary 
            "Hexadecimal" -> Mode.Hexadecimal
            _ -> Mode.Decimal
    in Just $ ModeChange mode a

rowDataUpdateState :: State -> Int -> String -> State
rowDataUpdateState state index datum = 
    case Int.fromString $ String.trim datum of 
        Nothing ->
            state { refreshCount = state.refreshCount + 1}
        Just num -> 
            case Array.updateAt (index - 1) num state.data of
                Nothing ->
                    state { refreshCount = state.refreshCount + 1}
                Just x ->
                    state { data = x }

