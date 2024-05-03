module Pages.Info where

import Prelude

import Deku.Core (fixed, text_)
import Deku.DOM as D
import Deku.DOM.Attributes as DA
import Deku.Ionic.BackButton as IBB
import Deku.Ionic.Buttons as IBS
import Deku.Ionic.Content as IC
import Deku.Ionic.Header as IH
import Deku.Ionic.Router (ionRoute_)
import Deku.Ionic.Title as ITi
import Deku.Ionic.Toolbar as IT

info :: _
info = ionRoute_ @{ name :: String } \_ { name } -> fixed
  [ IH.ionHeader_
      [ IT.ionToolbar_
          [ IBS.ionButtons [ DA.slot_ "start" ]
              [ IBB.ionBackButton [ IBB.defaultHref_ "/" ] []
              ]
          , ITi.ionTitle_
              [ text_ "Info"
              ]
          ]
      ]
  , IC.ionContent [ DA.klass_ "ion-padding" ]
      [ D.p__ $ "hi " <> name <> "!"
      , D.p__ "Ok, we lied, there isn't more info..."
      ]
  ]
