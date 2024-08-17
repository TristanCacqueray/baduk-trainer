-- | Some helper function to create bootstrap element
module Bootstrap where

import Data.Maybe (Maybe)
import Halogen.HTML (PropName(..))
import Halogen.HTML as HH
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Prelude (($), (<>))
import Web.UIEvent.MouseEvent (MouseEvent)

type HTML w i
  = HH.HTML w i

div :: forall w i. String -> Array (HTML w i) -> HTML w i
div className body =
  HH.div
    [ HP.class_ (ClassName className) ]
    body

card :: forall w i. String -> Array (HTML w i) -> HTML w i
card header body = div "card" [ div "card-body" [ div "card-title" [ HH.text header ], div "card-text" body ] ]

a :: forall w i. String -> String -> HTML w i
a link name = HH.a [ HP.href link ] [ HH.text name ]

center :: forall w i. Array (HTML w i) -> HTML w i
center = div "text-center"

spinner :: forall w i. HTML w i
spinner = div "spinner-border m-5" []

alert :: forall w i. String -> Array (HTML w i) -> HTML w i
alert level = div ("alert alert-" <> level)

alertDanger :: forall w i. Array (HTML w i) -> HTML w i
alertDanger = alert "danger"

row :: forall w i. Array (HTML w i) -> HTML w i
row = div "row"

button :: forall w i. String -> String -> (MouseEvent -> i) -> HTML w i
button level body cb = HH.a [ HP.class_ (ClassName $ "btn btn-" <> level), HE.onClick cb ] [ HH.text body ]

canvas :: forall w i. String -> Int -> HTML w i
canvas id size =
  HH.canvas
    [ HP.id id
    , HP.width size
    , HP.height size
    , HP.prop (PropName "style") "border: 1px solid black"
    ]
