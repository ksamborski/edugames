module Multiplication.View exposing (calculationView, init, setUpAnimator, update)

import Animator
import Browser.Dom as Dom
import Element
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Element.Keyed as Keyed
import Html.Attributes as Html
import Keyboard exposing (Key(..))
import Keyboard.Events as Keyboard
import List.Extra exposing (getAt, setAt, transpose, zip)
import Math.Multiplication exposing (..)
import Multiplication.Types exposing (..)
import Task


init : Int -> Int -> ( Model, Cmd Msg )
init n n2 =
    let
        op =
            emptyModel.currentOperation

        rowLen =
            List.length (decimals n) + List.length (decimals n2)

        emptyRow =
            List.repeat rowLen Nothing

        newFocus =
            FocusedResultRow (rowLen - 1) 0
    in
    ( { emptyModel
        | currentOperation =
            { op
                | multiplicand = n
                , multiplier = n2
                , sumUpperRow = emptyRow
                , finalResult = emptyRow
            }
        , focused = newFocus
      }
    , Task.attempt (\_ -> NoOp) (Dom.focus <| colId <| focused2ColId newFocus)
    )


setUpAnimator : (m -> Model) -> (m -> Model -> m) -> Animator.Animator m -> Animator.Animator m
setUpAnimator get set =
    Animator.watchingWith
        (.errors << get)
        (\newErrors m ->
            let
                model =
                    get m
            in
            set m { model | errors = newErrors }
        )
        (\merrors ->
            case merrors of
                Nothing ->
                    False

                _ ->
                    True
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg m =
    let
        op =
            m.currentOperation

        multiplicandDigits =
            decimals m.currentOperation.multiplicand

        multiplierDigits =
            decimals m.currentOperation.multiplier
    in
    case msg of
        UpperRowInput row col mval ->
            let
                newUpperRows =
                    case getAt row op.upperRows of
                        Just r ->
                            setAt row (setAt col mval r) op.upperRows

                        Nothing ->
                            List.append op.upperRows [ setAt col mval <| List.repeat (List.length multiplicandDigits + 1) Nothing ]
            in
            ( { m | currentOperation = { op | upperRows = newUpperRows } }, Cmd.none )

        ResultRowInput row col mval ->
            let
                newResultRows =
                    case getAt row op.resultRows of
                        Just r ->
                            setAt row (setAt col mval r) op.resultRows

                        Nothing ->
                            List.append
                                op.resultRows
                                [ setAt col mval <|
                                    List.repeat
                                        (List.length multiplicandDigits + List.length multiplierDigits)
                                        Nothing
                                ]
            in
            ( { m | currentOperation = { op | resultRows = newResultRows } }, Cmd.none )

        SumUpperRowInput col mval ->
            let
                newSumUpperRow =
                    setAt col mval op.sumUpperRow
            in
            ( { m | currentOperation = { op | sumUpperRow = newSumUpperRow } }, Cmd.none )

        FinalResultRowInput col mval ->
            let
                newFinalResult =
                    setAt col mval op.finalResult
            in
            ( { m | currentOperation = { op | finalResult = newFinalResult } }, Cmd.none )

        CheckResult ->
            let
                nonzeroUpperRows =
                    transpose <|
                        fixedRows True 0 Nothing <|
                            List.map (List.filter (\mv -> mv /= Just 0 && mv /= Nothing)) <|
                                transpose op.upperRows

                newOp =
                    { op | upperRows = nonzeroUpperRows }
            in
            ( { m
                | currentOperation = newOp
                , checked = True
                , errors =
                    Animator.interrupt
                        [ Animator.event Animator.immediately (errors (input2multiplication newOp))
                        , Animator.wait (Animator.millis 950)
                        , Animator.event Animator.immediately Nothing
                        ]
                        m.errors
              }
            , Cmd.none
            )

        Focus dir ->
            let
                newFocused =
                    changeFocus op m.focused dir
            in
            ( { m | focused = newFocused }
            , Task.attempt (\_ -> NoOp) (Dom.focus <| colId <| focused2ColId newFocused)
            )

        ForceFocus cid ->
            ( { m | focused = colId2focused cid }, Cmd.none )

        NoOp ->
            ( m, Cmd.none )


upperRowStyle : List (Element.Attribute Msg)
upperRowStyle =
    [ Font.size 12
    , Font.color (Element.rgb255 200 10 10)
    , Element.htmlAttribute (Html.style "direction" "rtl")
    , Element.htmlAttribute (Html.style "text-align" "center")
    , Element.htmlAttribute (Html.style "height" "20px")
    , Element.alignRight
    ]


resultRowStyle : List (Element.Attribute Msg)
resultRowStyle =
    [ Font.size 20
    , Font.color (Element.rgb255 10 10 200)
    , Element.htmlAttribute (Html.style "direction" "rtl")
    , Element.htmlAttribute (Html.style "text-align" "center")
    , Element.alignRight
    ]


zipWithDefault : a -> b -> List a -> List b -> List ( a, b )
zipWithDefault defa defb a b =
    let
        aLen =
            List.length a

        bLen =
            List.length b
    in
    if bLen > aLen then
        zip (a ++ List.repeat (bLen - aLen) defa) b

    else
        zip a (b ++ List.repeat (aLen - bLen) defb)


calculationView : Model -> Element.Element Msg
calculationView m =
    let
        multiplicandDigits =
            decimals m.currentOperation.multiplicand

        multiplierDigits =
            decimals m.currentOperation.multiplier

        digitsColsNum =
            List.length multiplicandDigits + 1

        resultColsNum =
            List.length multiplierDigits + List.length multiplicandDigits

        animatedInputRow =
            renderInputRow m.errors

        resultRowsLen =
            max
                (List.length m.currentOperation.resultRows)
                (List.length diff.resultRows)

        upperRowsLen =
            max
                (List.length m.currentOperation.upperRows)
                (List.length diff.upperRows)

        lastLine mop =
            animatedInputRow
                { rid = ResultRow resultRowsLen
                , numEl = resultColsNum
                , style = resultRowStyle
                , operator = mop
                , action = ResultRowInput resultRowsLen
                }
                []
                Nothing

        moreThan1ResultRow =
            List.length m.currentOperation.resultRows
                > 1
                || List.length diff.resultRows
                > 1

        diff =
            case Animator.current m.errors of
                Nothing ->
                    { resultRows = List.repeat (List.length m.currentOperation.resultRows) Nothing
                    , upperRows = List.repeat (List.length m.currentOperation.upperRows) Nothing
                    , sumUpperRow = Nothing
                    , finalResult = Nothing
                    }

                Just d ->
                    { resultRows = List.map Just d.resultRows
                    , upperRows = List.map Just d.upperRows
                    , sumUpperRow = Just d.sumUpperRow
                    , finalResult = Just d.finalResult
                    }
    in
    Element.el
        [ Element.width Element.fill
        , Element.height Element.fill
        ]
    <|
        Keyed.column []
            (( rowId (UpperRow upperRowsLen)
             , animatedInputRow
                { rid = UpperRow upperRowsLen
                , numEl = digitsColsNum
                , style = upperRowStyle
                , operator = Nothing
                , action = UpperRowInput upperRowsLen
                }
                []
                Nothing
             )
                :: List.reverse
                    (List.indexedMap
                        (\i ( r, d ) ->
                            ( rowId (UpperRow i)
                            , animatedInputRow
                                { rid = UpperRow i
                                , numEl = digitsColsNum
                                , style = upperRowStyle
                                , operator = Nothing
                                , action = UpperRowInput i
                                }
                                r
                                d
                            )
                        )
                     <|
                        zipWithDefault [] Nothing m.currentOperation.upperRows diff.upperRows
                    )
                ++ [ ( "multiplicand", textNumber multiplicandDigits )
                   , ( "multiplier", operationLine "×" [ textNumber multiplierDigits ] )
                   ]
                ++ (if moreThan1ResultRow then
                        [ ( rowId SumUpperRow
                          , animatedInputRow
                                { rid = SumUpperRow
                                , numEl = resultColsNum
                                , style = upperRowStyle
                                , operator = Nothing
                                , action = SumUpperRowInput
                                }
                                m.currentOperation.sumUpperRow
                                diff.sumUpperRow
                          )
                        ]

                    else
                        []
                   )
                ++ (List.indexedMap
                        (\i ( r, d ) ->
                            ( rowId (ResultRow i)
                            , animatedInputRow
                                { rid = ResultRow i
                                , numEl = resultColsNum
                                , style = resultRowStyle
                                , operator = Nothing
                                , action = ResultRowInput i
                                }
                                r
                                d
                            )
                        )
                    <|
                        zipWithDefault [] Nothing m.currentOperation.resultRows diff.resultRows
                   )
                ++ (if moreThan1ResultRow then
                        [ ( rowId (ResultRow resultRowsLen), lastLine (Just "+") )
                        , ( rowId FinalRow
                          , animatedInputRow
                                { rid = FinalRow
                                , numEl = resultColsNum
                                , style = resultRowStyle
                                , operator = Nothing
                                , action = FinalResultRowInput
                                }
                                m.currentOperation.finalResult
                                diff.finalResult
                          )
                        ]

                    else
                        [ ( rowId (ResultRow resultRowsLen), lastLine Nothing ) ]
                   )
            )


operatorRowStyle : List (Element.Attribute Msg)
operatorRowStyle =
    [ Border.widthEach { bottom = 1, left = 0, right = 0, top = 0 }
    , Border.solid
    ]


operatorEl : String -> Element.Element Msg
operatorEl op =
    Element.el [ Element.width (Element.px 20) ] <| Element.text op


operationLine : String -> List (Element.Element Msg) -> Element.Element Msg
operationLine operator children =
    Element.row
        (operatorRowStyle
            ++ [ Element.spacing 0
               , Element.width Element.fill
               ]
        )
        (operatorEl operator :: children)


type alias RenderInputRowOptions =
    { rid : RowId
    , style : List (Element.Attribute Msg)
    , numEl : Int
    , action : Int -> Maybe Int -> Msg
    , operator : Maybe String
    }


renderInputRow :
    Animator.Timeline state
    -> RenderInputRowOptions
    -> List (Maybe Int)
    -> Maybe (List CheckedDigit)
    -> Element.Element Msg
renderInputRow timeline opts elements mdiff =
    Keyed.row
        ([ Element.width Element.fill
         , Font.variant Font.tabularNumbers
         , Element.spacing 0
         , Element.padding 0
         ]
            ++ (case opts.operator of
                    Just op ->
                        operatorRowStyle

                    Nothing ->
                        []
               )
        )
    <|
        (case opts.operator of
            Just op ->
                [ ( "operator" ++ op, operatorEl op ) ]

            Nothing ->
                []
        )
            ++ (List.indexedMap
                    (\idx ( mn, d ) ->
                        let
                            cid =
                                ColId opts.rid idx

                            elId =
                                colId cid
                        in
                        ( elId
                        , Input.text
                            ([ Element.width (Element.px 20)
                             , Font.color (Element.rgb255 200 10 10)
                             , Element.height (Element.px 20)
                             , Element.padding 0
                             , Element.pointer
                             , Element.focused [ Background.color (Element.rgba 1 1 1 0.5) ]
                             , Element.mouseOver [ Background.color (Element.rgba 1 1 1 0.25) ]
                             , Events.onFocus <| ForceFocus cid
                             , Element.htmlAttribute <| Html.id elId
                             , Element.htmlAttribute
                                (Keyboard.on Keyboard.Keydown
                                    [ ( ArrowUp, Focus FocusUp )
                                    , ( ArrowDown, Focus FocusDown )
                                    , ( ArrowLeft, Focus FocusLeft )
                                    , ( ArrowRight, Focus FocusRight )
                                    , ( Enter, CheckResult )
                                    ]
                                )
                             , Background.color
                                (if isOk d then
                                    Element.rgba 1 1 1 0

                                 else
                                    Element.rgba 1 0 0 <|
                                        Animator.linear timeline <|
                                            \_ ->
                                                Animator.wave 0 0.65 |> Animator.loop (Animator.millis 900)
                                )
                             , Border.width 0
                             ]
                                ++ opts.style
                            )
                            { onChange = opts.action idx << Maybe.map (remainderBy 10) << String.toInt
                            , text = Maybe.withDefault "" <| Maybe.map String.fromInt mn
                            , placeholder = Nothing
                            , label = Input.labelHidden ""
                            }
                        )
                    )
                <|
                    zip
                        (List.repeat (opts.numEl - List.length elements) Nothing
                            ++ elements
                        )
                        (case mdiff of
                            Nothing ->
                                List.repeat opts.numEl (IsOk 0)

                            Just diff ->
                                List.repeat (opts.numEl - List.length diff) (IsOk 0) ++ diff
                        )
               )


textNumber : List Int -> Element.Element Msg
textNumber digits =
    Element.row [ Element.width Element.fill, Font.variant Font.tabularNumbers ] <|
        List.map
            (\d ->
                Element.el [ Font.center, Element.alignRight, Element.width (Element.px 20) ] <|
                    Element.text <|
                        String.fromInt d
            )
            digits
