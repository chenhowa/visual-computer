module TextDisplay where
  
import Data.Either
import Effect.Console
import Prelude

import Data.Array as Array
import Data.Foldable (foldr)
import Data.Function.Uncurried as F
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), length, split, splitAt, lastIndexOf, indexOf)
import Data.Tuple as Tuple
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML (a)
import Halogen.HTML as HH
import Halogen.HTML.Core as HC
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import TextDisplayLine as TextDisplayLine
import TextSelection (setSelection, startIndex, endIndex)
import WhatUtils (classes, constructNewText, isPrintable, backspaceText, deleteText)
import Web.Event.Event (Event, preventDefault)
import Web.HTML.Event.EventTypes (offline)
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.MouseEvent as ME




type State = Program
type Program =
    { text :: String
    , selectionStart :: Int
    , selectionEnd :: Int
    }

type Input = String

data Query a 
    = KeyDown String Int Int a
    | PreventDefault Event (Query a)
    | NoOp a
    | Click a
    | SetSelection a

data Message 
    = TextChanged
    | NonprintableKey

data Slot = LineSlot Int
derive instance eqTextLineSlot :: Eq Slot
derive instance ordTextLineSlot :: Ord Slot

type ChildQuery = TextDisplayLine.Query

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
        initialState = 
            { text: "DEFAUL\nTTTO12345\njklj"
            , selectionStart: 0
            , selectionEnd: 5
            }

        render :: State -> H.ParentHTML Query ChildQuery Slot Aff
        render state = 
            HH.div
                [ classes [ "text-display-component" ]
                , HP.title "HI"
                , HH.attr (HH.AttrName "contenteditable") "true"
                , HE.onKeyDown \e -> Just $ PreventDefault (KE.toEvent e) $ H.action (handleKeyDown e)
                --, HE.onKeyUp \e -> Just $ PreventDefault (KE.toEvent e) $ H.action (NoOp)
                --, HE.onClick \e -> Just $ PreventDefault (ME.toEvent e) $ H.action (NoOp)
                --, HE.onMouseDown \e -> Just $ PreventDefault (ME.toEvent e) $ H.action (NoOp)
                ]
                children
            where 
                lines = split (Pattern "\n") state.text
                slots = LineSlot <$> Array.range 1 (Array.length lines)
                pairs = Array.zip slots lines
                children = renderPair <$> pairs
                renderPair tup = HH.slot (Tuple.fst tup) TextDisplayLine.component ( (Tuple.snd tup) <> "\n") absurd
            -- WEIRD. DO I NEED TO INSERT "\n" ??  and clearly the selection algorithm is wrong here for 
            -- for some reason.
        eval :: Query ~> H.ParentDSL State Query ChildQuery Slot Message Aff
        eval q = case q of 
            KeyDown newChar start end next ->
                if isPrintable newChar
                then do
                    { text, selectionStart, selectionEnd} <- H.get
                    let newText = (constructNewText text start end newChar)
                    H.liftEffect $ log $ "newText: " <> newText
                    H.put $ 
                        { text: newText
                        , selectionStart: start + 1
                        , selectionEnd: start + 1
                        }
                    H.raise TextChanged
                    pure next
                else do 
                    H.liftEffect $ log ("nonprintable: " <> newChar)
                    state@{ text, selectionStart, selectionEnd} <- H.get
                    let newState = getNewStateFromNonprintable state start end newChar
                    H.put newState
                    H.raise NonprintableKey
                    pure next
            PreventDefault event nextQuery -> do
                H.liftEffect $ preventDefault event
                eval nextQuery
            NoOp next -> do pure next
            Click next -> do
                pure next
            SetSelection next -> do
                H.liftEffect $ log "WHATO"
                {text, selectionStart, selectionEnd} <- H.get
                let _ = F.runFn3 setSelection "text-display-component" selectionStart selectionEnd
                pure next
 
        handleKeyDown :: forall a. KE.KeyboardEvent -> a -> Query a
        handleKeyDown event = 
            let
                key = KE.key event
                code = KE.code event
                start = startIndex "text-editor-component"
                end = endIndex "text-editor-component"
            in
                KeyDown key (min start end) (max start end)

getNewStateFromNonprintable :: State -> Int -> Int -> String -> State
getNewStateFromNonprintable state start end nonprintable = case nonprintable of 
    "ArrowRight" -> 
        if start == end 
        then if start < (length state.text)
            then state { selectionStart = start + 1, selectionEnd = end + 1}
            else state
        else state { selectionStart = end, selectionEnd = end }
    "ArrowLeft" -> 
        if start == end 
        then if start > 0
            then state { selectionStart = start - 1, selectionEnd = end - 1}
            else state
        else state { selectionStart = start, selectionEnd = start }
    "ArrowUp" -> getArrowUpState state start end
    "ArrowDown" -> getArrowDownState state start end
    "Backspace" ->
        let newText = backspaceText state.text start end
        in if start > 0
            then state 
                { text = newText
                , selectionStart = if start == end then start - 1 else start
                , selectionEnd = if start == end then end - 1 else start
                }
            else state
                { text = newText
                , selectionStart = 0
                , selectionEnd = 0
                }
    "Delete" ->
        let newText = deleteText state.text start end 
            newState = state 
                { text = newText 
                , selectionStart = start
                , selectionEnd = start
                }
        in if end < length state.text
            then newState
            else newState
    _ -> state

getArrowUpState :: State -> Int -> Int -> State
getArrowUpState state start end = 
    let before = (splitAt start state.text).before
        maybeLastIndex = lastIndexOf (Pattern "\n") before
        offset = case maybeLastIndex of 
            Nothing -> 0
            Just index -> (length before) - index
        maybePreviousIndex = case maybeLastIndex of 
            Nothing -> Just 0
            Just index -> lastIndexOf (Pattern "\n") (splitAt index before).before 
        newStart = case maybePreviousIndex of 
            Nothing -> max (offset - 1) 0
            Just 0 -> 0
            Just index -> case maybeLastIndex of 
                Nothing -> index + offset
                Just lastIndex -> min lastIndex (index + offset)
    in 
        state 
            { selectionStart = newStart 
            , selectionEnd = newStart
            }

getArrowDownState :: State -> Int -> Int -> State
getArrowDownState state start end = 
    let split = (splitAt end state.text) -- THIS IS WRONG. OFFSET NEEDS TO BE COUNTED FROM START OF LINE, NOT END.
        after = split.after 
        before = split.before
        maybeLastNewlineIndex = lastIndexOf (Pattern "\n") before
        offset = case maybeLastNewlineIndex of
            Nothing -> end 
            Just index -> (length before) - index
        maybeNextNewlineIndex = indexOf (Pattern "\n") after
        newEnd = case maybeNextNewlineIndex of 
            Nothing -> length state.text
            Just index -> (index + offset)

{-
        maybeFirstIndex = indexOf (Pattern "\n") after
        offset = case maybeFirstIndex of 
            Nothing -> 0
            Just index -> index 
        maybeNextIndex = case maybeFirstIndex of 
            Nothing -> Just $ length state.text
            Just index -> firstIndexOf (Pattern "\n") (splitAt index after).after 
        newEnd = case maybeNextIndex of 
            Nothing -> min offset (length state.text)
            Just $ length state.text -> length state.text 
            Just index -> index - offset-}
    in state
        { selectionStart = newEnd
        , selectionEnd = newEnd
        }