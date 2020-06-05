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
        ( lastIdx, lastRowLen, rows ) =
            List.foldl
                (\r ( idx, _, acc ) ->
                    ( idx + 1, List.length r.resultRows, remainderRowView nCols idx r :: acc )
                )
                ( 0, 0, [] )
                remainders
    in
    Keyed.column [] <|
        List.reverse <|
            List.concat <|
                (if lastIdx == 0 || lastRowLen > 1 then
                    remainderRowView nCols (lastIdx + 1) Division.emptyRemainderRowInput

                 else
                    []
                )
                    :: rows


remainderRowView : Int -> Int -> Division.RemainderRowInput -> List ( String, Element.Element Division.Msg )
remainderRowView nCols idx rowInput =
    let
        rowId i =
            "remainderResultRow" ++ String.fromInt idx ++ "," ++ String.fromInt i

        inputRow i vals =
            ( rowId i
            , numberInputRow
                { onChange = Division.ChangeRemainderResult idx i
                , maxDigits = 1
                , id = rowId i ++ ","
                }
                vals
            )

        ( lastIdx, rows ) =
            List.foldl
                (\r ( ridx, acc ) -> ( ridx + 1, inputRow ridx r :: acc ))
                ( 0, [] )
                rowInput.resultRows
    in
    List.reverse <|
        inputRow lastIdx (Array.repeat nCols Nothing)
            :: rows
