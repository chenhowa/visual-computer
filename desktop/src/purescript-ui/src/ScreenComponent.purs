module ScreenComponent where

import Prelude
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import Effect (Effect)
import Effect.Console (log)

main2 :: Effect Unit
main2 = do
  log "Hello sailor!"
