module Division.View exposing (calculationView)

import Array exposing (Array)
import Division.Types as Division
import Element
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Element.Keyed as Keyed
import Math.Utils as Math
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
            , ( "remainderRows", remainderRowsView m.currentOperation.remainderRows )
            ]


resultRowView : Array (Maybe Int) -> Element.Element Division.Msg
resultRowView row =
    Theme.numberRow [] <|
        List.map
            (\( idx, mv ) ->
                Theme.numberInput
                    { onChange = Division.ChangeResult idx
                    , maxDigits = 1
                    , value = mv
                    , id = "result" ++ String.fromInt idx
                    }
                    []
            )
        <|
            Array.toIndexedList row


operationRowView : Int -> Int -> Element.Element Division.Msg
operationRowView dividend divisor =
    Theme.numberRow
        [ Border.widthEach { bottom = 0, left = 0, right = 0, top = 1 } ]
    <|
        (List.map (Theme.numberText [] << String.fromInt) <| Math.decimals dividend)
            ++ [ Theme.numberText [] ""
               , Theme.numberText [] ":"
               , Theme.numberText [] ""
               ]
            ++ (List.map (Theme.numberText [] << String.fromInt) <| Math.decimals divisor)


remainderRowsView : List Division.RemainderRowInput -> Element.Element Division.Msg
remainderRowsView remainders =
    Element.none
