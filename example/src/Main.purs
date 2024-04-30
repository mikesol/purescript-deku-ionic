module Main where

import Prelude

import App (app)
import Deku.Toplevel (runInBody)
import Effect (Effect)
import Pages.Info (info)
import Pages.Intro (intro)
  
main :: Effect Unit
main = do
  intro
  info
  runInBody  app