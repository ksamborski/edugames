module Theme.Math exposing (centerXbyCols, gridBackground, numberInput, numberRow, numberText)

import Element
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
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


numberRow : List (Element.Attribute msg) -> List (Element.Element msg) -> Element.Element msg
numberRow atrs =
    Element.row
        ([ Element.width Element.fill
         , Font.variant Font.tabularNumbers
         ]
            ++ atrs
        )


numberText : List (Element.Attribute msg) -> String -> Element.Element msg
numberText atrs n =
    Element.el
        ([ Font.center
         , Element.alignRight
         , Element.width (Element.px 20)
         ]
            ++ atrs
        )
    <|
        Element.text n


numberInput :
    { onChange : Maybe Int -> msg, maxDigits : Int, value : Maybe Int, id : String }
    -> List (Element.Attribute msg)
    -> Element.Element msg
numberInput opts attrs =
    Input.text
        ([ Element.width (Element.px 20)
         , Font.color (Element.rgb255 200 10 10)
         , Element.height (Element.px 20)
         , Element.padding 0
         , Element.pointer
         , Element.focused [ Background.color (Element.rgba 1 1 1 0.5) ]
         , Element.mouseOver [ Background.color (Element.rgba 1 1 1 0.25) ]
         , Element.htmlAttribute <| Html.id opts.id
         , Element.htmlAttribute <| Html.attribute "inputmode" "numeric"
         , Element.htmlAttribute <| Html.pattern ("[0-9]{0," ++ String.fromInt opts.maxDigits ++ "}")
         , Background.color (Element.rgba 1 1 1 0)
         , Border.width 0
         ]
            ++ attrs
        )
        { onChange = opts.onChange << Maybe.map (remainderBy <| 10 ^ opts.maxDigits) << String.toInt
        , text = Maybe.withDefault "" <| Maybe.map String.fromInt opts.value
        , placeholder = Nothing
        , label = Input.labelHidden ""
        }
