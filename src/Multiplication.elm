module Multiplication exposing (Multiplication, correctResult, decimals, errors, main)

import Browser
import Element
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes as Html
import List.Extra exposing (cartesianProduct, dropWhile, getAt, groupsOf, interweave, lift2, setAt, transpose, zip)
import Random


type alias Model =
    { currentOperation : MultiplicationInput
    , minNumOfDigits : Int
    , maxNumOfDigits : Int
    }


type alias MultiplicationInput =
    { multiplicand : Int
    , multiplier : Int

    -- ^ user input
    , resultRows : List (List (Maybe Int))
    , upperRows : List (List (Maybe Int))

    -- ^ sums
    , sumUpperRow : List (Maybe Int)
    , finalResult : List (Maybe Int)
    }


type alias Multiplication =
    { multiplicand : Int
    , multiplier : Int

    -- ^ user input
    , resultRows : List (List Int)
    , upperRows : List (List Int)

    -- ^ sums
    , sumUpperRow : List Int
    , finalResult : List Int
    }


type Step
    = Calculate ( Int, Int )
    | SaveRow Int


type Msg
    = NewMultiplication ( Int, Int )
    | UpperRowInput Int Int (Maybe Int)
    | ResultRowInput Int Int (Maybe Int)


emptyModel : Model
emptyModel =
    { currentOperation = emptyMultiplicationInput
    , minNumOfDigits = 1
    , maxNumOfDigits = 2
    }


emptyMultiplication : Multiplication
emptyMultiplication =
    { multiplicand = 0
    , multiplier = 0
    , resultRows = []
    , upperRows = []
    , sumUpperRow = []
    , finalResult = []
    }


emptyMultiplicationInput : MultiplicationInput
emptyMultiplicationInput =
    { multiplicand = 0
    , multiplier = 0
    , resultRows = []
    , upperRows = []
    , sumUpperRow = []
    , finalResult = []
    }


multNumGenerator : Int -> Int -> Random.Generator Int
multNumGenerator digMin digMax =
    let
        numMin =
            if digMin <= 1 then
                0

            else
                10 ^ (digMin - 1)

        numMax =
            if digMax <= 1 then
                0

            else
                10 ^ (digMax + 1) - 1
    in
    Random.int numMin numMax


correctResult : Int -> Int -> Multiplication
correctResult n1 n2 =
    multiply n1 n2
        |> sumResults


sumResults : Multiplication -> Multiplication
sumResults m =
    let
        maxResultNumDig =
            Maybe.withDefault 0 <|
                List.maximum <|
                    List.map List.length m.resultRows

        resultsWithZeros =
            List.map
                (\row -> List.repeat (maxResultNumDig - List.length row) 0 ++ row)
                m.resultRows

        result =
            Tuple.first <|
                List.foldl
                    runSumStep
                    ( m, 0 )
                    ((List.reverse <| transpose resultsWithZeros) ++ [ [] ])

        trFinalResult =
            case dropWhile ((==) 0) result.finalResult of
                [] ->
                    [ 0 ]

                r ->
                    r
    in
    { result | finalResult = trFinalResult }


runSumStep : List Int -> ( Multiplication, Int ) -> ( Multiplication, Int )
runSumStep nums ( acc, rest ) =
    let
        s =
            List.sum nums + rest

        rmd =
            s // 10
    in
    case nums of
        [] ->
            if rest == 0 then
                ( acc, 0 )

            else
                ( { acc | finalResult = s :: acc.finalResult }, 0 )

        _ ->
            ( { acc
                | sumUpperRow =
                    if rmd /= 0 then
                        rmd :: acc.sumUpperRow

                    else
                        0 :: acc.sumUpperRow
                , finalResult = remainderBy 10 s :: acc.finalResult
              }
            , rmd
            )


multiply : Int -> Int -> Multiplication
multiply n1 n2 =
    let
        { multStage } =
            List.foldl
                runMultiplicationStep
                { multStage = { emptyMultiplication | multiplicand = n1, multiplier = n2 }
                , resultRow = []
                , upperRow = []
                , rest = 0
                }
                (multiplicationSteps n1 n2)
    in
    multStage


runMultiplicationStep :
    Step
    -> { multStage : Multiplication, resultRow : List Int, upperRow : List Int, rest : Int }
    -> { multStage : Multiplication, resultRow : List Int, upperRow : List Int, rest : Int }
runMultiplicationStep op acc =
    case op of
        Calculate ( n, m ) ->
            let
                mult =
                    n * m + acc.rest

                newRest =
                    mult // 10
            in
            { multStage = acc.multStage
            , resultRow = remainderBy 10 mult :: acc.resultRow
            , upperRow = newRest :: acc.upperRow
            , rest = newRest
            }

        SaveRow zeros ->
            let
                mult =
                    acc.multStage

                resultSignDigits =
                    if acc.rest > 0 then
                        acc.rest :: acc.resultRow

                    else
                        acc.resultRow
            in
            { multStage =
                { mult
                    | resultRows = (resultSignDigits ++ List.repeat zeros 0) :: mult.resultRows
                    , upperRows = acc.upperRow :: mult.upperRows
                }
            , resultRow = []
            , upperRow = []
            , rest = 0
            }


multiplicationSteps : Int -> Int -> List Step
multiplicationSteps n1 n2 =
    let
        decN1 =
            List.reverse <| decimals n1

        decN2 =
            List.reverse <| decimals n2
    in
    cartesianProduct [ decN2, decN1 ]
        |> List.map
            (\input ->
                case input of
                    [ n, m ] ->
                        Calculate ( n, m )

                    _ ->
                        Calculate ( 0, 0 )
            )
        |> groupsOf (List.length decN1)
        |> (\l ->
                interweave l (List.indexedMap (\i f -> [ f i ]) <| List.repeat (List.length decN2) SaveRow)
                    |> List.concat
           )


decimals : Int -> List Int
decimals num =
    let
        dec n acc =
            if n > -10 && n < 10 then
                n :: acc

            else
                dec (n // 10) (remainderBy 10 n :: acc)
    in
    dec num []


type CheckedDigit
    = IsOk Int
    | IsWrong Int Int


type alias AnnotatedMultiplication =
    { multiplicand : Int
    , multiplier : Int

    -- ^ user input
    , resultRows : List (List CheckedDigit)
    , upperCols : List (List CheckedDigit)

    -- ^ sums
    , sumUpperRow : List CheckedDigit
    , finalResult : List CheckedDigit
    }


emptyAnnotatedMultiplication : AnnotatedMultiplication
emptyAnnotatedMultiplication =
    { multiplicand = 0
    , multiplier = 0
    , resultRows = []
    , upperCols = []
    , sumUpperRow = []
    , finalResult = []
    }


equalLenList : a -> List a -> List a -> ( List a, List a )
equalLenList filler lst1 lst2 =
    let
        lst1len =
            List.length lst1

        lst2len =
            List.length lst2
    in
    if lst1len > lst2len then
        ( lst1, List.repeat (lst1len - lst2len) filler ++ lst2 )

    else
        ( List.repeat (lst2len - lst1len) filler ++ lst1, lst2 )


errors : Multiplication -> Maybe AnnotatedMultiplication
errors m =
    let
        correct =
            correctResult m.multiplicand m.multiplier

        diff =
            { emptyAnnotatedMultiplication
                | multiplicand = m.multiplicand
                , multiplier = m.multiplier
                , resultRows =
                    List.map (\( a, b ) -> diffList a b) <| zip givenResRows wantedResRows
                , upperCols =
                    List.map
                        (\( a, b ) -> diffList (List.filter ((/=) 0) a) (List.filter ((/=) 0) b))
                    <|
                        zip (transpose <| fixedRows 0 givenUpRows) (transpose <| fixedRows 0 wantedUpRows)
                , sumUpperRow =
                    diffList m.sumUpperRow correct.sumUpperRow
                , finalResult =
                    diffList m.finalResult correct.finalResult
            }

        ( givenResRows, wantedResRows ) =
            equalLenList [] m.resultRows correct.resultRows

        ( givenUpRows, wantedUpRows ) =
            equalLenList [] m.upperRows correct.upperRows

        ok =
            List.all identity
                [ List.all (List.all isOk) diff.resultRows
                , List.all (List.all isOk) diff.upperCols
                , List.all isOk diff.sumUpperRow
                , List.all isOk diff.finalResult
                ]
    in
    if ok then
        Nothing

    else
        Just diff


fixedRows : a -> List (List a) -> List (List a)
fixedRows filler lst =
    let
        upto =
            Maybe.withDefault 0 <| List.maximum <| List.map List.length lst
    in
    List.map (\l -> List.repeat (upto - List.length l) filler ++ l) lst


isOk : CheckedDigit -> Bool
isOk d =
    case d of
        IsOk _ ->
            True

        _ ->
            False


diffList : List Int -> List Int -> List CheckedDigit
diffList given wanted =
    equalLenList 0 given wanted
        |> (\( l2, r2 ) ->
                List.map
                    (\( le, re ) ->
                        if le == re then
                            IsOk le

                        else
                            IsWrong le re
                    )
                    (zip l2 r2)
           )


main : Program () Model Msg
main =
    Browser.element
        { init =
            \_ ->
                ( emptyModel
                , Random.generate
                    NewMultiplication
                    (Random.pair (multNumGenerator 1 3) (multNumGenerator 1 3))
                )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg m =
    let
        op =
            m.currentOperation

        multiplicandDigits =
            decimals m.currentOperation.multiplicand
    in
    case msg of
        NewMultiplication ( n, n2 ) ->
            ( { m | currentOperation = { op | multiplicand = n, multiplier = n2 } }, Cmd.none )

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
                            List.append op.resultRows [ setAt col mval <| List.repeat (List.length multiplicandDigits + 1) Nothing ]
            in
            ( { m | currentOperation = { op | resultRows = newResultRows } }, Cmd.none )


view : Model -> Html Msg
view m =
    Element.layout
        [ Background.color (Element.rgb255 220 220 220)
        , Element.htmlAttribute <|
            Html.style "background-image" "linear-gradient(rgba(255,255,255,.3) 1px, transparent 1px), linear-gradient(90deg, rgba(255,255,255,.3) 1px, transparent 1px)"
        , Element.htmlAttribute <|
            Html.style "background-size" "20px 20px, 20px 20px, 20px 20px, 20px 20px"
        , Element.htmlAttribute <|
            Html.style "background-position" "-2px -2px, -2px -2px, -1px -1px, -1px -1px"
        ]
        (calculationView m)


upperRowStyle : List (Element.Attribute Msg)
upperRowStyle =
    [ Font.size 12
    , Font.color (Element.rgb255 200 10 10)
    , Element.htmlAttribute (Html.style "direction" "rtl")
    , Element.htmlAttribute (Html.style "text-align" "center")
    , Element.htmlAttribute (Html.style "height" "20px")
    ]


resultRowStyle : List (Element.Attribute Msg)
resultRowStyle =
    [ Font.size 20
    , Font.color (Element.rgb255 10 10 200)
    , Element.htmlAttribute (Html.style "direction" "rtl")
    , Element.htmlAttribute (Html.style "text-align" "center")
    ]


calculationView : Model -> Element.Element Msg
calculationView m =
    let
        multiplicandDigits =
            decimals m.currentOperation.multiplicand

        multiplierDigits =
            decimals m.currentOperation.multiplier

        digitsColsNum =
            List.length multiplicandDigits + 1
    in
    Element.el
        [ Element.width Element.fill
        , Element.height Element.fill
        ]
    <|
        Element.column []
            (renderInputRow upperRowStyle digitsColsNum [] (UpperRowInput (List.length m.currentOperation.upperRows))
                :: List.reverse (List.indexedMap (\i r -> renderInputRow upperRowStyle digitsColsNum r <| UpperRowInput i) m.currentOperation.upperRows)
                ++ [ textNumber multiplicandDigits
                   , Element.row
                        [ Border.widthEach { bottom = 1, left = 0, right = 0, top = 0 }
                        , Border.solid
                        , Element.spacing 0
                        , Element.width Element.fill
                        ]
                        [ Element.el [ Element.width (Element.px 20) ] <| Element.text "×"
                        , textNumber multiplierDigits
                        ]
                   ]
                ++ List.indexedMap (\i r -> renderInputRow resultRowStyle digitsColsNum r <| ResultRowInput i) m.currentOperation.resultRows
                ++ [ renderInputRow resultRowStyle digitsColsNum [] (ResultRowInput (List.length m.currentOperation.resultRows)) ]
            )


renderInputRow : List (Element.Attribute Msg) -> Int -> List (Maybe Int) -> (Int -> Maybe Int -> Msg) -> Element.Element Msg
renderInputRow style numEl elements action =
    Element.row
        [ Element.width Element.fill
        , Font.variant Font.tabularNumbers
        , Element.spacing 0
        , Element.padding 0
        ]
    <|
        List.indexedMap
            (\idx mn ->
                Input.text
                    ([ Element.width (Element.px 20)
                     , Font.color (Element.rgb255 200 10 10)
                     , Element.height (Element.px 20)
                     , Element.padding 0
                     , Element.pointer
                     , Element.focused [ Background.color (Element.rgba 1 1 1 0.5) ]
                     , Element.mouseOver [ Background.color (Element.rgba 1 1 1 0.25) ]
                     , Background.color (Element.rgba 1 1 1 0)
                     , Border.width 0
                     ]
                        ++ style
                    )
                    { onChange = action idx << Maybe.map (remainderBy 10) << String.toInt
                    , text = Maybe.withDefault "" <| Maybe.map String.fromInt mn
                    , placeholder = Nothing
                    , label = Input.labelHidden ""
                    }
            )
        <|
            List.repeat (numEl - List.length elements) Nothing
                ++ elements


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
