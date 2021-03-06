module TextEditor.TextDisplay where
  
import Data.Either
import Effect.Console
import Prelude

import Data.Array as Array
import Data.Foldable (foldr)
import Data.Function.Uncurried as F
import Data.Maybe (Maybe(..))
import Data.MediaType.Common (textPlain)
import Data.String (Pattern(..), length, split)
import Data.Tuple as Tuple
import Effect as Ef
import Effect.Aff (Aff)
import Events.UI as EUI
import Halogen as H
import Halogen.HTML (a)
import Halogen.HTML as HH
import Halogen.HTML.Core as HC
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import TextEditor.TextDisplayLine as TextDisplayLine
import TextSelection as TS
import Web.Clipboard.ClipboardEvent as CE
import Web.Event.Event (Event, preventDefault)
import Web.HTML.Event.DataTransfer as DT
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.MouseEvent as ME
import WhatUtils as U


componentClass :: String 
componentClass = "text-display-component"

type State = Program
type Program =
    { text :: String
    , selectionStart :: Int
    , selectionEnd :: Int
    }

type Input = String

data Query a 
    = UnmodifiedKeyDown String Int Int a
    | GeneralKeyDown KE.KeyboardEvent a
    | PreventDefault Event (Query a)
    | NoOp a
    | Click a
    | SetSelection a
    | Paste (Ef.Effect String) Int Int a
    | Cut (Maybe DT.DataTransfer) Int Int a
    | Initialize a
    | HandleTripleClick (H.SubscribeStatus -> a)
    | HandleDoubleClick a

data Message 
    = TextChanged
    | NonprintableKey

data Slot = LineSlot Int
derive instance eqTextLineSlot :: Eq Slot
derive instance ordTextLineSlot :: Ord Slot

type ChildQuery = TextDisplayLine.Query

component :: H.Component HH.HTML Query Input Message Aff 
component =
    H.lifecycleParentComponent 
        { initialState: const initialState
        , render
        , eval
        , receiver: const Nothing
        , initializer: Just $ H.action Initialize
        , finalizer: Nothing
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
                [ U.classes [ componentClass ]
                , HP.title "HI"
                , HH.attr (HH.AttrName "contenteditable") "true"
                , HE.onKeyDown $ HE.input handleGeneralKeyDown
                , HE.onPaste $ \e -> Just $ PreventDefault (CE.toEvent e) $ H.action (handlePaste e)
                , HE.onCut $ \e -> Just $ PreventDefault (CE.toEvent e) $ H.action (handleCut e)
                , HE.onDoubleClick $ HE.input_ HandleDoubleClick
                --, HE.onKeyDown \e -> Just $ PreventDefault (KE.toEvent e) $ H.action (handleKeyDown e)
                --, HE.onKeyUp \e -> Just $ PreventDefault (KE.toEvent e) $ H.action (NoOp)
                --, HE.onClick \e -> Just $ PreventDefault (ME.toEvent e) $ H.action (NoOp)
                --, HE.onMouseDown \e -> Just $ PreventDefault (ME.toEvent e) $ H.action (NoOp)
                ]
                children
            where 
                lines = split (Pattern "\n") state.text 
                numbers = Array.range 1 (Array.length lines)  
                pairs :: Array (Tuple.Tuple Int String)
                pairs = Array.zip numbers lines
                children = renderPair <$> pairs
                renderPair tup = HH.slot (LineSlot $ Tuple.fst tup) TextDisplayLine.component ({ text: Tuple.snd tup, number: Tuple.fst tup }) absurd
        eval :: Query ~> H.ParentDSL State Query ChildQuery Slot Message Aff
        eval q = case q of 
            Initialize next -> do
                H.liftEffect $ log case EUI.target componentClass of 
                        Just target -> "found target"
                        Nothing -> "couldn't find target!"
                case EUI.target componentClass of 
                        Just target -> H.subscribe $ H.eventSource_ (EUI.onTripleClick target) (H.request HandleTripleClick)
                        Nothing -> pure unit
                pure next
            GeneralKeyDown event next ->
                if isModified event
                then eval $ handleModifiedKeyDown event $ next
                else eval (PreventDefault (KE.toEvent event) $ handleUnmodifiedKeyDown event $ next  )
            UnmodifiedKeyDown newChar start end next ->
                if U.isPrintable newChar
                then do
                    state <- H.get
                    H.put $ newStateFromPrintable state start end newChar
                    H.raise TextChanged
                    pure next
                else do 
                    state@{ text, selectionStart, selectionEnd} <- H.get
                    let newState = newStateFromNonprintable state start end newChar
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
                {text, selectionStart, selectionEnd} <- H.get
                H.liftEffect $ log $ "set start: " <> show selectionStart
                H.liftEffect $ log $ "set end: " <> show selectionEnd
                let _ = F.runFn3 TS.setSelection componentClass selectionStart selectionEnd
                pure next
            Paste d start end next -> do
                pasted <- H.liftEffect d
                H.liftEffect $ log pasted
                state <- H.get
                H.put $ pasteState state start end pasted
                H.raise TextChanged
                pure next
            Cut d start end next -> do
                state <- H.get 
                H.liftEffect case d of 
                    Just dt -> DT.setData textPlain (U.copyString state.text start end) dt 
                    Nothing -> pure unit
                H.put $ cutState state start end
                H.raise TextChanged
                pure next
            HandleTripleClick reply -> do
                H.liftEffect $ log "RUNNING"
                let indices = TS.cursorIndices componentClass
                let start = indices.start
                let end = indices.end
                H.liftEffect $ log $ "triple start: " <> show start
                H.liftEffect $ log $ "triple end: " <> show end
                state <- H.get 
                let newState = tripleClickState state start end
                H.liftEffect $ log $ "triple start 2: " <> show newState.selectionStart
                H.liftEffect $ log $ "triple end 2: " <> show newState.selectionEnd
                H.put $ newState
                H.raise TextChanged
                pure (reply H.Listening)
            HandleDoubleClick next -> do 
                let indices = TS.cursorIndices componentClass 
                state <- H.get
                H.put $ doubleClickState state indices.start indices.end
                H.raise TextChanged
                pure next 

        isModified :: KE.KeyboardEvent -> Boolean 
        isModified event = KE.ctrlKey event || KE.altKey event || KE.metaKey event

        handleModifiedKeyDown :: forall a. KE.KeyboardEvent -> a -> Query a
        handleModifiedKeyDown event
            | otherwise = NoOp
             
        handleUnmodifiedKeyDown :: forall a. KE.KeyboardEvent -> a -> Query a
        handleUnmodifiedKeyDown event = 
            let
                key = KE.key event
                code = KE.code event
                indices = TS.cursorIndices componentClass
                start = indices.start
                end = indices.end
            in
                UnmodifiedKeyDown key (min start end) (max start end)

        handleGeneralKeyDown :: forall a. KE.KeyboardEvent -> a -> Query a 
        handleGeneralKeyDown ke = GeneralKeyDown ke

        handlePaste :: forall a. CE.ClipboardEvent -> a -> Query a
        handlePaste ce =
            let d = case CE.clipboardData ce of 
                        Just clipData -> DT.getData textPlain clipData
                        Nothing -> pure ""
                indices = TS.cursorIndices componentClass
                start = indices.start
                end = indices.end
            in Paste d (min start end) (max start end) 

        handleCut :: forall a. CE.ClipboardEvent -> a -> Query a 
        handleCut ce =
            let d = CE.clipboardData ce
                indices = TS.cursorIndices componentClass
                start = indices.start
                end = indices.end
            in Cut d (min start end) (max start end)

newStateFromPrintable :: State -> Int -> Int -> String -> State
newStateFromPrintable state start end newChar = 
    let newText = (U.constructNewText state.text start end newChar)
    in  { text: newText
        , selectionStart: start + 1
        , selectionEnd: start + 1
        }

newStateFromNonprintable :: State -> Int -> Int -> String -> State
newStateFromNonprintable state start end nonprintable = case nonprintable of 
    "ArrowRight" -> arrowRightState state start end
    "ArrowLeft" -> arrowLeftState state start end
    "ArrowUp" -> arrowUpState state start end
    "ArrowDown" -> arrowDownState state start end
    "Backspace" -> backspaceState state start end
    "Delete" -> deleteState state start end
    _ -> state

arrowUpState :: State -> Int -> Int -> State
arrowUpState state start end = 
    let newStart = U.arrowUpIndex state.text start end
    in 
        state 
            { selectionStart = newStart 
            , selectionEnd = newStart
            }

arrowDownState :: State -> Int -> Int -> State
arrowDownState state start end = 
    let newEnd = U.arrowDownIndex state.text start end
    in state
        { selectionStart = newEnd
        , selectionEnd = newEnd
        }

arrowLeftState :: State -> Int -> Int -> State 
arrowLeftState state start end = 
    let newStart = U.arrowLeftIndex state.text start end 
    in state 
        { selectionStart = newStart 
        , selectionEnd = newStart 
        }

arrowRightState :: State -> Int -> Int -> State 
arrowRightState state start end = 
    let newEnd = U.arrowRightIndex state.text start end 
    in state 
        { selectionStart = newEnd 
        , selectionEnd = newEnd
        }

backspaceState :: State -> Int -> Int -> State 
backspaceState state start end =
    let newText = U.backspaceText state.text start end
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

deleteState :: State -> Int -> Int -> State
deleteState state start end = 
    let newText = U.deleteText state.text start end 
        newState = state 
            { text = newText 
            , selectionStart = start
            , selectionEnd = start
            }
    in if end < length state.text
        then newState
        else newState

pasteState :: State -> Int -> Int -> String -> State
pasteState state start end insert =
    let newEnd = start + length insert
        newState = 
            { text: U.pasteString state.text start end insert
            , selectionStart: newEnd
            , selectionEnd: newEnd
            }
    in newState

cutState :: State -> Int -> Int -> State 
cutState state start end = 
    let newStart = start 
        newState = 
            { text: U.backspaceText state.text start end 
            , selectionStart: newStart
            , selectionEnd: newStart
            }
    in newState
 
tripleClickState :: State -> Int -> Int -> State 
tripleClickState state start end =
    let newStart = 
            if start == 0
            then start
            else 
                case U.charAt start state.text of 
                    Nothing -> start
                    Just char -> if char == "\n" then start + 1 else start
        newEnd = min (length state.text) (end + 1)
    in  state
          { selectionStart = newStart
          , selectionEnd = newEnd
          }

doubleClickState :: State -> Int -> Int -> State 
doubleClickState state start end = 
    let newStart = if start /= 0 then start else start
        newEnd = end
    in state 
        { selectionStart = newStart 
        , selectionEnd = newEnd
        }

