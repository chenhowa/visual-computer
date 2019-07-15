module TextSelection where

import Data.Function.Uncurried as F

foreign import startIndex :: String -> Int


foreign import endIndex :: String -> Int


foreign import selection :: forall a. a -> String

foreign import setSelection :: F.Fn3 String Int Int Int
--foreign import setSelection :: String -> Int -> Int -> Int