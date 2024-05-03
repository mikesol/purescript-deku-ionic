module App where

import Prelude
import Deku.Core (Nut)
import Deku.Ionic.App as IA
import Deku.Ionic.Router (ionRouter, useHash_)
import Deku.Ionic.RouterOutlet as IRO
import Effect (Effect)
import Pages.Info (info)
import Pages.Intro (intro)

app :: Effect Nut
app = do
  rtr <- ionRouter [ useHash_ false ]
    { "/": intro
    , "/info": info
    }
  pure $ IA.ionApp_
    [ rtr
    , IRO.ionRouterOutlet_ []
    ]
