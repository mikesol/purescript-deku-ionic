module Pages.Intro where

import Prelude

import Deku.Core (fixed, text_)
import Deku.DOM.Attributes as DA
import Deku.Ionic.Button as IB
import Deku.Ionic.Card as ICard
import Deku.Ionic.CardContent as ICC
import Deku.Ionic.CardHeader as ICH
import Deku.Ionic.CardSubtitle as ICS
import Deku.Ionic.CardTitle as ICT
import Deku.Ionic.Content as IC
import Deku.Ionic.Header as IH
import Deku.Ionic.Router (ionRoute_)
import Deku.Ionic.Title as ITi
import Deku.Ionic.Toolbar as IT
import FRP.Poll (Poll)

intro :: _
intro = ionRoute_ @{} \{ "/info": info } _ -> fixed
  [ IH.ionHeader_
      [ IT.ionToolbar_
          [ ITi.ionTitle_
              [ text_ "Intro"
              ]
          ]
      ]
  , IC.ionContent [ DA.klass_ "ion-padding" ]
      [ ICard.ionCard_
          [ ICH.ionCardHeader_
              [ ICT.ionCardTitle_
                  [ text_ "Oh hi"
                  ]
              , ICS.ionCardSubtitle_
                  [ text_ "This is deku using ionic"
                  ]
              ]
          , ICC.ionCardContent_
              [ text_ "Here's a small text description for the card content. Nothing more, nothing less."
              ]
          ]
      , info (pure { name: "Mike" } :: Poll _) []
          [ IB.ionButton_
              [ text_ "More info"
              ]
          ]
      ]
  ]
