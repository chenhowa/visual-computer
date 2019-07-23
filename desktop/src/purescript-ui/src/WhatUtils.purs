module WhatUtils where

import Prelude

import Data.String (splitAt, length, drop, take)
import Halogen.HTML.Core as HC
import Halogen.HTML.Properties as HP
import Data.String (Pattern(..), length, split, splitAt, lastIndexOf, indexOf, uncons, fromCodePointArray)
import Data.Maybe (Maybe(..))



classes :: forall t4 t5. Array String -> HP.IProp (class :: String | t5 ) t4
classes list = HP.classes $ HC.ClassName <$> list

constructNewText :: String -> Int -> Int -> String -> String
constructNewText full start end insert = 
    let transformedInsert = transformKey insert
    in
        if start == end
        then (splitAt start full).before <> transformedInsert <> (splitAt start full).after
        else (splitAt start full).before <> transformedInsert <> (splitAt end full).after

transformKey :: String -> String
transformKey str = case str of 
    "Enter" -> "\n"
    "Tab" -> "\t"
    _ -> str

isPrintable :: String -> Boolean
isPrintable str = if length str == 1
    then true 
    else if isNotPrintable str
        then false 
        else true
    where 
        isNotPrintable :: String -> Boolean
        isNotPrintable s = case s of 
            "Enter" -> false
            "Tab" -> false
            _ -> true

backspaceText :: String -> Int -> Int -> String
backspaceText full start end =
    if start == end 
    then 
        let before = (splitAt start full).before
            after = (splitAt end full).after
        in if start > length full || end < 0 
            then full 
            else (take ((length before) - 1) before) <> after
    else (splitAt start full).before <> (splitAt end full).after

deleteText :: String -> Int -> Int -> String
deleteText full start end = 
    if start == end 
    then 
        backspaceText full (start + 1) (start + 1)
    else (splitAt start full).before <> (splitAt end full).after


arrowUpIndex :: String -> Int -> Int -> Int 
arrowUpIndex full start end = 
    if start /= end
    then  (min (max start 0) $ length full)
    else 
        let before = (splitAt start full).before
            maybeIndex = do
                lastNewlineIndex <- getLastNewlineIndex before
                let lineOffset = calculateOffset lastNewlineIndex $ length before
                let lastLastNewlineIndex = case getLastLastNewlineIndex lastNewlineIndex before of 
                        Nothing -> (-1)
                        Just lastLast -> lastLast
                Just $ getNewStart lastNewlineIndex lastLastNewlineIndex lineOffset
        in case maybeIndex of
            Nothing -> 0 -- could not find the index -- so go to start of string
            Just index -> index
    where 
        getLastNewlineIndex :: String -> Maybe Int 
        getLastNewlineIndex str = lastIndexOf (Pattern "\n") str

        calculateOffset :: Int -> Int -> Int 
        calculateOffset index total = total - index

        getLastLastNewlineIndex :: Int -> String -> Maybe Int
        getLastLastNewlineIndex first str = lastIndexOf (Pattern "\n") (splitAt first str).before 

        getNewStart :: Int -> Int -> Int -> Int  
        getNewStart last lastLast offset = min last $ lastLast + offset

arrowDownIndex :: String -> Int -> Int -> Int 
arrowDownIndex full start end = 
    if start /= end 
    then (max (min end $ length full) 0)
    else 
        let split = (splitAt end full)
            after = split.after 
            before = split.before
            maybeIndex = do 
                let lastNewlineIndex = case getLastNewlineIndex before of 
                        Nothing -> (-1)
                        Just i -> i
                let lineOffset = calculateOffset lastNewlineIndex $ length before
                nextNewlineIndex <- getNextNewlineIndex end full
                let nextNextNewlineIndex = case getNextNextNewlineIndex nextNewlineIndex full of 
                        Nothing -> length full
                        Just nextNext -> nextNext
                Just $ getNewEnd nextNewlineIndex nextNextNewlineIndex lineOffset
        in case maybeIndex of 
            Nothing -> length full -- could not find the index -- so go to end of string
            Just index -> index
    where
        getLastNewlineIndex :: String -> Maybe Int 
        getLastNewlineIndex str = lastIndexOf (Pattern "\n") str

        calculateOffset :: Int -> Int -> Int 
        calculateOffset index total = total - index

        getNextNewlineIndex :: Int -> String -> Maybe Int 
        getNextNewlineIndex current str = 
            let after = (splitAt current str).after
            in  case indexOf (Pattern "\n") after of 
                    Nothing -> Nothing 
                    Just index -> Just $ current + index

        getNextNextNewlineIndex :: Int -> String -> Maybe Int 
        getNextNextNewlineIndex current str = 
            let after = (splitAt (current + 1) str).after
            in  case indexOf (Pattern "\n") after of 
                    Nothing -> Nothing
                    Just index -> Just $ current + 1 + index

        getNewEnd :: Int -> Int -> Int -> Int 
        getNewEnd next nextNext offset = min (nextNext) $ next + offset

arrowLeftIndex :: String -> Int -> Int -> Int 
arrowLeftIndex str start end = 
    if start /= end 
    then min (length str) $ max start 0
    else min (length str) $ max (start - 1) 0

arrowRightIndex :: String -> Int -> Int -> Int 
arrowRightIndex str start end = 
    if start /= end 
    then max 0 $ min end $ length str
    else max 0 $ min (end + 1) $ length str

pasteString :: String -> Int -> Int -> String -> String
pasteString full start end insert = 
    if (not $ validIndex start full) || (not $ validIndex end full)
    then full
    else 
        if start /= end 
        then 
            let before = (splitAt start full).before
                after = (splitAt end full).after
            in before <> insert <> after
        else
            let before = (splitAt start full).before
                after = (splitAt start full).after 
            in  before <> insert <> after

copyString :: String -> Int -> Int -> String
copyString full start end = 
    if (not $ validIndex start full) || (not $ validIndex end full)
    then ""
    else 
        if start /= end
        then take (end - start) $ drop start full 
        else ""

validIndex :: Int -> String -> Boolean
validIndex index str = index >= 0 && index <= (length str)

charAt :: Int -> String -> Maybe String
charAt index str = 
    let after = (splitAt index str).after
    in case uncons after of 
            Nothing -> Nothing
            Just { head, tail } -> Just $ fromCodePointArray [head]

hiddenClasses :: Boolean -> Array String 
hiddenClasses show = if show then [] else ["hidden"]