module Test.UtilSpec where

import Prelude

import Data.String (length)
import Test.Spec (pending, describe, it, Spec)
import Test.Spec.Assertions (shouldEqual)
import WhatUtils as U

spec :: Spec Unit
spec = 
    describe "Test of utility functions" do
        describe "cursor positioning calculations" do
            describe "arrow up logic" do
                let long1 = "0123456789"
                let long2 = "0123456789"
                let short = "01234"
                let string = long1 <> "\n" <> short <> "\n" <> long2
                let len = length string
                describe "range" do 
                    it "goes to the start index if the start index is valid" do 
                        U.arrowUpIndex string 2 (len) `shouldEqual` 2
                    it "goes to the closest valid index if the start index is not valid" do 
                        U.arrowUpIndex string (-1) (len) `shouldEqual` 0
                        U.arrowUpIndex string (len + 1) (len + 3) `shouldEqual` len
                describe "single cursor" do 
                    describe "handles the common case where the index is the same in the current and previous lines" do 
                        it "second to first line" do
                            let thirdOnSecond = (length long1 + 3)
                            let thirdOnFirst = 2
                            U.arrowUpIndex string thirdOnSecond thirdOnSecond `shouldEqual` thirdOnFirst
                        it "third to second line" do
                            let thirdOnThird = (length long1 + length short + 1 + 3)
                            let thirdOnSecond = (length long1 + 3)
                            U.arrowUpIndex string thirdOnThird thirdOnThird `shouldEqual`thirdOnSecond
                    it "handles being in the first line by going to the beginning of the line" do 
                        U.arrowUpIndex string 2 2 `shouldEqual` 0
                    it "goes to the end of the short line when the cursor is originally at the end of a long line" do 
                        U.arrowUpIndex string len len `shouldEqual` (length long1 + length short + 1)
                        U.arrowUpIndex string (len - 1) (len - 1) `shouldEqual` (length long1 + length short + 1)
            describe "arrow down logic" do
                let long1 = "0123456789"
                let long2 = "0123456789"
                let short = "01234"
                let string = long1 <> "\n" <> short <> "\n" <> long2
                let len = length string
                describe "range" do 
                    it "goes to the end index if the end index is valid" do 
                        U.arrowDownIndex string 2 (len) `shouldEqual` len
                    it "goes to the closest valid index if the end index is not valid" do 
                        U.arrowDownIndex string (-2) (-1) `shouldEqual` 0
                        U.arrowDownIndex string (len + 1) (len + 3) `shouldEqual` len
                describe "single cursor" do 
                    describe "handles the common case where the index is the same in the current and previous lines" do 
                        it "first to second line" do
                            let thirdOnFirst = 2
                            let thirdOnSecond = (length long1 + 3)
                            U.arrowDownIndex string thirdOnFirst thirdOnFirst  `shouldEqual` thirdOnSecond
                        it "second to end line" do
                            let thirdOnSecond = (length long1 + 3)
                            let thirdOnLast = (length long1 + length short + 1 + 3)
                            U.arrowDownIndex string thirdOnSecond thirdOnSecond
                                `shouldEqual` thirdOnLast
                    it "handles being in the last line by going to the end of the line" do 
                        U.arrowDownIndex string (len - 3) (len - 3) `shouldEqual` len
                    describe "goes to the end of the short line when the cursor is originally at the end of a long line" do 
                        it "at end" do 
                            let endOfLong = (length long1)
                            let endOfShort = (length long1 + length short + 1)
                            U.arrowDownIndex string endOfLong endOfLong `shouldEqual` endOfShort
                        it "right before end" do 
                            let endOfLong = (length long1 - 1)
                            let endOfShort = (length long1 + length short + 1)
                            U.arrowDownIndex string endOfLong endOfLong `shouldEqual` endOfShort
            describe "arrow left logic" do 
                let string = "hello"
                let len = length string
                describe "with range" do 
                    it "collapses the range" do 
                        U.arrowLeftIndex string 1 3 `shouldEqual` 1
                    it "goes to closest valid index if start is not valid" do
                        U.arrowLeftIndex string (-1) 2 `shouldEqual` 0
                        U.arrowLeftIndex string (len + 1) (len + 2) `shouldEqual` len
                describe "with single cursor" do 
                    it "goes left one index if in middle of string" do 
                        U.arrowLeftIndex string 1 1 `shouldEqual` 0
                    it "goes to closest valid index if going left one is not valid" do
                        U.arrowLeftIndex string 0 0 `shouldEqual` 0
            describe "arrow right logic" do 
                let string = "hello"
                let len = length string
                describe "with range" do
                    it "collapses the range" do 
                        U.arrowRightIndex string 1 3 `shouldEqual` 3
                    it "goes to closest valid index if end is not valid" do 
                        U.arrowRightIndex string (-3) (-1) `shouldEqual` 0
                        U.arrowRightIndex string 2 (len + 1) `shouldEqual` len
                describe "with single cursor" do 
                    it "goes right one index if in middle of string" do 
                        U.arrowRightIndex string 1 1 `shouldEqual` 2
                    it "goes to closest valid index if going right one is not valid" do 
                        U.arrowRightIndex string len len `shouldEqual` len
        describe "text deletion calculations" do 
            describe "backspace logic" do 
                let string = "apples, \nbananas, gravy"
                let len = length string
                it "deletes one character in range" do 
                    U.backspaceText string 1 1 `shouldEqual` "pples, \nbananas, gravy"
                    U.backspaceText string len len `shouldEqual` "apples, \nbananas, grav"
                it "deletes two characters in range" do 
                    U.backspaceText string 0 2 `shouldEqual` "ples, \nbananas, gravy"
                    U.backspaceText string (len - 2) len `shouldEqual` "apples, \nbananas, gra"
                describe "returns the original string if not in range" do
                    it "one character out of range before" do 
                        U.backspaceText string (0) (0) `shouldEqual` string
                    it "one character out of range after" do
                        U.backspaceText string (len + 1) (len + 1) `shouldEqual` string
                    it "two characters out of range before" do 
                        U.backspaceText string (-2) (0) `shouldEqual` string
                        U.backspaceText string (len + 1) (len + 3) `shouldEqual` string
                it "does partial deletion if only some of the string is in range" do 
                    U.backspaceText string (-1) 1 `shouldEqual` "pples, \nbananas, gravy"
                    U.backspaceText string (len - 1) (len + 1) `shouldEqual` "apples, \nbananas, grav"
            describe "delete logic" do
                let string = "apples, \nbananas, gravy"
                let len = length string
                it "deletes one character in range" do 
                    U.deleteText string 0 0 `shouldEqual` "pples, \nbananas, gravy"
                    U.deleteText string (len - 1) (len - 1) `shouldEqual` "apples, \nbananas, grav"
                it "deletes two characters in range" do 
                    U.deleteText string 0 2 `shouldEqual` "ples, \nbananas, gravy"
                    U.deleteText string (len - 2) len `shouldEqual` "apples, \nbananas, gra"
                describe "returns the original string if not in range" do
                    it "one character out of range before" do 
                        U.deleteText string (-1) (-1) `shouldEqual` string
                    it "one character out of range after" do
                        U.deleteText string (len) (len) `shouldEqual` string
                    it "two characters out of range before" do 
                        U.deleteText string (-3) (-1) `shouldEqual` string
                        U.deleteText string (len) (len + 2) `shouldEqual` string
                it "does partial deletion if only some of the string is in range" do 
                    U.deleteText string (-1) 1 `shouldEqual` "pples, \nbananas, gravy"
                    U.deleteText string (len - 1) (len + 1) `shouldEqual` "apples, \nbananas, grav"
        describe "insertion logic" do
            describe "paste logic" do 
                describe "paste text" do
                    let string = "hello"
                    let len = length string
                    let insert = "w"
                    describe "range" do
                        it "at beginning" do 
                            U.pasteString string 0 1 insert `shouldEqual` SOMETHING
                        it "at middle" do
                            U.pasteString string 1 2 insert `shouldEqual` SOMETHING
                        it "at end" do
                            U.pasteString string (len - 1) len insert `shouldEqual` SOMETHING
                    describe "single cursor" do
                        pending "at beginning"
                        pending "at middle" 
                        pending "at end"
                    describe "invalid indices" do
                        pending "both indices are invalid"
                        pending "one index is invalid"

        describe "copy logic" do 
            describe "copy text" do
                describe "range" do 
                    pending "from middle"
                    pending "from beginning"
                    pending "from end"
                describe "single cursor" do 
                    pending "copying should not work at well"
                describe "invalid indices" do
                    pending "both indices are invalid"
                    pending "one index is invalid"

        describe "undo/redo logic" do 
            describe "undo" do 
                pending "with empty history"
                pending "with one item in history"
                describe "with two items in history" do
                    pending "at end"
                    pending "at beginning"
            describe "redo" do
                pending "with empty history"
                pending "with one item in history"
                describe "with two items in history" do
                    pending "at end"
                    pending "at beginning"

        describe "cut logic" do 
            describe "range" do 
                pending "test"
            describe "single cursor" do
                pending "should do nothing"
            describe "works with undo/redo" do 
                pending "test"