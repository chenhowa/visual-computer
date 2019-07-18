module TextSelection 
    ( cursorIndices
    , setSelection    
    ) where

import Prelude
import Data.Function.Uncurried as F

foreign import startIndex :: String -> Int


foreign import endIndex :: String -> Int


foreign import selection :: forall a. a -> String

foreign import setSelection :: F.Fn3 String Int Int Int

cursorIndices :: String -> { start :: Int, end :: Int }
cursorIndices str = 
    let start = startIndex str 
        end = endIndex str 
    in  { start: min start end
        , end: max start end
        }
