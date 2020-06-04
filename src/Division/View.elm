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
    let
        nCols =
            Array.length m.currentOperation.resultRow
    in
    Element.el
        [ Element.width Element.fill
        , Element.height Element.fill
        ]
    <|
        Keyed.column
            (Theme.centerXbyCols nCols w)
            [ ( "resultRow", resultRowView m.currentOperation.resultRow )
            , ( "operationRow", operationRowView m.currentOperation.dividend m.currentOperation.divisor )
            , ( "remainderRows", remainderRowsView nCols m.currentOperation.remainderRows )
            ]


numberInputRow :
    { onChange : Int -> Maybe Int -> Division.Msg, maxDigits : Int, id : String }
    -> Array (Maybe Int)
    -> Element.Element Division.Msg
numberInputRow opts row =
    Theme.numberRow [] <|
        List.map
            (\( idx, mv ) ->
                Theme.numberInput
                    { onChange = opts.onChange idx
                    , maxDigits = opts.maxDigits
                    , value = mv
                    , id = opts.id ++ String.fromInt idx
                    }
                    []
            )
        <|
            Array.toIndexedList row


resultRowView : Array (Maybe Int) -> Element.Element Division.Msg
resultRowView row =
    numberInputRow
        { onChange = Division.ChangeResult
        , maxDigits = 1
        , id = "result"
        }
        row


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


remainderRowsView : Int -> List Division.RemainderRowInput -> Element.Element Division.Msg
remainderRowsView nCols remainders =
    let
        ( lastIdx, rows ) =
            List.foldl
                (\r ( idx, acc ) ->
                    ( idx + 1, remainderRowView nCols idx r :: acc )
                )
                ( 0, [] )
                remainders
    in
    Keyed.column [] <|
        List.reverse <|
            List.concat <|
                remainderRowView nCols (lastIdx + 1) Division.emptyRemainderRowInput
                    :: rows


remainderRowView : Int -> Int -> Division.RemainderRowInput -> List ( String, Element.Element Division.Msg )
remainderRowView nCols idx r =
    []
