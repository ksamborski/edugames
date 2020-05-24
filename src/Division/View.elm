module Division.View exposing (calculationView)

import Array
import Division.Types as Division
import Element
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Element.Keyed as Keyed
import Theme.Math as Theme


calculationView : Division.Model -> Int -> Element.Element Division.Msg
calculationView m w =
    Element.el
        [ Element.width Element.fill
        , Element.height Element.fill
        ]
    <|
        Keyed.column
            (Theme.centerXbyCols (Array.length m.currentOperation.resultRow) w)
            [ ( "resultRow", resultRowView m.currentOperation.resultRow )
            , ( "operationRow", operationRowView m.currentOperation.dividend m.currentOperation.divisor )
            , ( "remainderRows", remainderRowsView currentOperation.remainderRows )
            ]


resultRowView : Array (Maybe Int) -> Element.Element Division.Msg
resultRowView row =
    Element.none


operationRowView : Int -> Int -> Element.Element Division.Msg
operationRowView dividend divisor =
    Element.none


remainderRowsView : List Division.RemainderRowInput -> Element.Element Division.Msg
remainderRowsView remainders =
    Element.none
