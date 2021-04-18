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
import Html.Attributes as Html
import Math.Utils as Math
import Theme.Math as Theme


upperRowStyle : List (Element.Attribute Division.Msg)
upperRowStyle =
    [ Font.size 12
    , Font.color (Element.rgb255 200 10 10)
    , Element.htmlAttribute (Html.style "direction" "rtl")
    , Element.htmlAttribute (Html.style "text-align" "center")
    , Element.htmlAttribute (Html.style "height" "20px")
    , Element.alignRight
    ]


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
    { onChange : Int -> Maybe Int -> Division.Msg, maxDigits : Int, id : String, style : List (Element.Attribute Division.Msg) }
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
                    opts.style
            )
        <|
            Array.toIndexedList row


opNumberInputRow :
    { op : String, onChange : Int -> Maybe Int -> Division.Msg, maxDigits : Int, id : String, style : List (Element.Attribute Division.Msg) }
    -> Array (Maybe Int)
    -> Element.Element Division.Msg
opNumberInputRow opts row =
    Theme.opNumberRow
        opts.op
        [ Border.widthEach { bottom = 1, left = 0, right = 0, top = 0 }
        , Border.solid
        ]
    <|
        List.map
            (\( idx, mv ) ->
                Theme.numberInput
                    { onChange = opts.onChange idx
                    , maxDigits = opts.maxDigits
                    , value = mv
                    , id = opts.id ++ String.fromInt idx
                    }
                    opts.style
            )
        <|
            Array.toIndexedList row


resultRowView : Array (Maybe Int) -> Element.Element Division.Msg
resultRowView row =
    numberInputRow
        { onChange = Division.ChangeResult
        , maxDigits = 1
        , id = "result"
        , style = []
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
        List.concat <|
            List.append rows <|
                [ if lastIdx == 0 || lastRowLen > 1 then
                    remainderRowView nCols (lastIdx + 1) Division.emptyRemainderRowInput

                  else
                    []
                ]


remainderRowView : Int -> Int -> Division.RemainderRowInput -> List ( String, Element.Element Division.Msg )
remainderRowView nCols idx rowInput =
    let
        ( uLastIdx, uRows ) =
            List.foldl
                (\r ( ridx, acc ) -> ( ridx + 1, remainderRowUpperRow idx ridx r :: acc ))
                ( 0, [] )
                rowInput.upperRows

        ( lastIdx, rows ) =
            List.foldl
                (\r ( ridx, acc ) -> ( ridx + 1, remainderRowResultRow idx ridx False r :: acc ))
                ( 0, [] )
                rowInput.resultRows

        upperRows =
            if List.length rows > 1 then
                remainderRowUpperRow idx uLastIdx (Array.repeat nCols Nothing) :: uRows

            else
                []

        resultRows =
            List.append rows [ remainderRowResultRow idx lastIdx (List.length rows > 1) (Array.repeat nCols Nothing) ]
    in
    List.append upperRows resultRows


remainderRowResultRow : Int -> Int -> Bool -> Division.RemainderRowInputRow -> ( String, Element.Element Division.Msg )
remainderRowResultRow y x withOpLine vals =
    let
        rowId =
            "remainderResultRow" ++ String.fromInt y ++ "," ++ String.fromInt x
    in
    ( rowId
    , if withOpLine then
        opNumberInputRow
            { onChange = Division.ChangeRemainderResult y x
            , maxDigits = 1
            , id = rowId ++ ","
            , style = []
            , op = "-"
            }
            vals

      else
        numberInputRow
            { onChange = Division.ChangeRemainderResult y x
            , maxDigits = 1
            , id = rowId ++ ","
            , style = []
            }
            vals
    )


remainderRowUpperRow : Int -> Int -> Division.RemainderRowInputRow -> ( String, Element.Element Division.Msg )
remainderRowUpperRow y x vals =
    let
        rowId =
            "remainderResultRowUpper" ++ String.fromInt y ++ "," ++ String.fromInt x
    in
    ( rowId
    , numberInputRow
        { onChange = Division.ChangeRemainderUpper y x
        , maxDigits = 1
        , id = rowId ++ ","
        , style = upperRowStyle
        }
        vals
    )
