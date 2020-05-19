module Theme.Math exposing (gridBackground)

import Element
import Element.Background as Background
import Html.Attributes as Html


gridBackground : List (Element.Attribute msg)
gridBackground =
    [ Background.color (Element.rgb255 220 220 220)
    , Element.htmlAttribute <|
        Html.style "background-image" "linear-gradient(rgba(255,255,255,.3) 1px, transparent 1px), linear-gradient(90deg, rgba(255,255,255,.3) 1px, transparent 1px)"
    , Element.htmlAttribute <|
        Html.style "background-size" "20px 20px, 20px 20px, 20px 20px, 20px 20px"
    , Element.htmlAttribute <|
        Html.style "background-position" "-2px -2px, -2px -2px, -1px -1px, -1px -1px"
    ]
