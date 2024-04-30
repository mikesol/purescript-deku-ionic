module Pages.Info where

import Prelude

import Deku.Core (text_)
import Deku.DOM.Attributes as DA
import Deku.Ionic.BackButton as IBB
import Deku.Ionic.Buttons as IBS
import Deku.Ionic.Content as IC
import Deku.Ionic.Custom (customComponent_)
import Deku.Ionic.Header as IH
import Deku.Ionic.Title as ITi
import Deku.Ionic.Toolbar as IT
import Effect (Effect)

info :: Effect Unit
info = customComponent_ "info-page" {} \_ ->
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
      [ text_ "Ok, we lied, there isn't more info..."
      ]
  ]
