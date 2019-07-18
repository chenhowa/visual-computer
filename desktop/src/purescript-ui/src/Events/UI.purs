module UI 
    ( onTripleClick
    , target 
    , Target
    ) where 

import Prelude

import Data.Function.Uncurried as UC
import Effect (Effect)
import Data.Maybe (Maybe(..))


foreign import data Target :: Type

foreign import data MaybeUndefined :: Type -> Type

foreign import onTripleClickImpl :: forall ev a. UC.Fn2 Target (ev -> Effect a) (Effect Unit)

foreign import isUndefined :: forall a. MaybeUndefined a -> Boolean

foreign import maybeTarget :: String -> MaybeUndefined Target

foreign import unsafeGetDefined :: forall a. MaybeUndefined a -> a

onTripleClick :: forall a. Target -> Effect a -> Effect Unit
onTripleClick self fn = UC.runFn2 onTripleClickImpl self (\_ -> fn)

target :: String -> Maybe Target
target str = 
    let t = maybeTarget str
    in
        if isUndefined t
        then Nothing
        else Just $ unsafeGetDefined t


