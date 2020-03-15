module Multiplication exposing (Multiplication, correctResult, decimals, main)

import Browser
import Html
import List.Extra exposing (cartesianProduct, groupsOf, interweave, lift2, transpose)
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
    | SaveRow


type Msg
    = Msg


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
multNumGenerator min max =
    Random.int min max


correctResult : Int -> Int -> Multiplication
correctResult n1 n2 =
    multiply n1 n2
        |> sumResults


sumResults : Multiplication -> Multiplication
sumResults m =
    Tuple.first <|
        List.foldl
            runSumStep
            ( m, 0 )
            ((List.reverse <| transpose m.resultRows) ++ [ [] ])


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

        SaveRow ->
            let
                mult =
                    acc.multStage
            in
            { multStage =
                { mult
                    | resultRows =
                        if acc.rest > 0 then
                            (acc.rest :: acc.resultRow) :: mult.resultRows

                        else
                            acc.resultRow :: mult.resultRows
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
                interweave l (List.repeat (List.length decN2) [ SaveRow ])
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
    , upperRows : List (List CheckedDigit)

    -- ^ sums
    , sumUpperRow : List CheckedDigit
    , finalResult : List CheckedDigit
    }


emptyAnnotatedMultiplication : AnnotatedMultiplication
emptyAnnotatedMultiplication =
    { multiplicand = 0
    , multiplier = 0
    , resultRows = []
    , upperRows = []
    , sumUpperRow = []
    , finalResult = []
    }


checkResult : Multiplication -> AnnotatedMultiplication
checkResult m =
    -- TODO the actual checking
    { emptyAnnotatedMultiplication | multiplicand = m.multiplicand, multiplier = m.multiplier }


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( emptyModel, Cmd.none )
        , view = \_ -> Html.div [] []
        , update = \_ m -> ( m, Cmd.none )
        , subscriptions = \_ -> Sub.none
        }
