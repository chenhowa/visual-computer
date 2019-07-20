module MemoryEditor.Mode 
    ( Mode(..)
    , intToString
    ) where 

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Int as Int

data Mode
    = Binary 
    | Hexadecimal
    | Decimal

derive instance genericMode :: Generic Mode _

instance showMode :: Show Mode where 
    show = genericShow


intToString :: Mode -> Int -> String
intToString m i = case m of 
    Binary -> Int.toStringAs Int.binary i
    Hexadecimal -> Int.toStringAs Int.hexadecimal i
    Decimal -> show i