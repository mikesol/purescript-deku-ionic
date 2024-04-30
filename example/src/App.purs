module App where

import Deku.Core (Nut)
import Deku.Ionic.App as IA
import Deku.Ionic.Route as IR
import Deku.Ionic.Router as IRT
import Deku.Ionic.RouterOutlet as IRO

app :: Nut
app = IA.ionApp_
  [ IRT.ionRouter_
      [ IR.ionRoute [ IR.url_ "/", IR.component_ "intro-page" ] []
      , IR.ionRoute [ IR.url_ "/info", IR.component_ "info-page" ] []
      ]
  , IRO.ionRouterOutlet_ []
  ]