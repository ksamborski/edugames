module Theme.Math exposing (centerXbyCols, gridBackground)

import Element
import Element.Background as Background
import Html.Attributes as Html


centerXbyCols : Int -> Int -> List (Element.Attribute msg)
centerXbyCols cols w =
    let
        width =
            w - remainderBy 40 w

        paddingX =
            if remainderBy 2 cols == 0 then
                (width - cols * 20) // 2

            else
                (width - (cols + 1) * 20) // 2
    in
    if w <= 0 then
        [ Element.centerX, Element.paddingXY 0 19 ]

    else
        [ Element.paddingXY paddingX 19 ]


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
