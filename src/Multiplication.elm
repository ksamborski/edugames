module Multiplication exposing (Multiplication, correctResult, decimals, errors, main)

import Browser
import Element
import Element.Background as Background
import Element.Font as Font
import Html exposing (Html)
import Html.Attributes as Html
import List.Extra exposing (cartesianProduct, dropWhile, groupsOf, interweave, lift2, transpose, zip)
import Random


type alias Model =
    { currentOperation : Multiplication
    , minNumOfDigits : Int
    , maxNumOfDigits : Int
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


emptyModel : Model
emptyModel =
    { currentOperation = emptyMultiplication
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
    in
    case msg of
        NewMultiplication ( n, n2 ) ->
            ( { m | currentOperation = { op | multiplicand = n, multiplier = n2 } }, Cmd.none )


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


calculationView : Model -> Element.Element Msg
calculationView m =
    Element.el
        [ Element.width Element.fill
        , Element.height Element.fill
        ]
    <|
        Element.column []
            [ textNumber (decimals m.currentOperation.multiplicand)
            , textNumber (decimals m.currentOperation.multiplier)
            ]


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
