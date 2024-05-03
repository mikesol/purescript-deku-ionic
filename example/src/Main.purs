module Main where

import Prelude

import App (app)
import Deku.Toplevel (runInBody)
import Effect (Effect)

main :: Effect Unit
main = do
  app >>= runInBody