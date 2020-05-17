module Math.Division exposing (Division, correctResult, emptyDivision)

import Math.Substraction as Substraction exposing (Substraction)
import Math.Utils as Math


type alias Division =
    { dividend : Int
    , divisor : Int
    , result : Int
    , remainderRows : List Substraction
    }


correctResult : Int -> Int -> Division
correctResult dividend divisor =
    { emptyDivision
        | dividend = dividend
        , divisor = divisor
        , result = dividend // divisor
        , remainderRows = runSteps (Math.decimals dividend) divisor
    }


runSteps : List Int -> Int -> List Substraction
runSteps dividend divisor =
    List.reverse <| runStepsRecursively dividend divisor []


runStepsRecursively : List Int -> Int -> List Substraction -> List Substraction
runStepsRecursively dividend divisor subs =
    case runStep dividend divisor of
        Just ( s, rest ) ->
            runStepsRecursively rest divisor (s :: subs)

        Nothing ->
            subs


runStep : List Int -> Int -> Maybe ( Substraction, List Int )
runStep dividend divisor =
    let
        ( n, rest ) =
            List.foldl
                (\el ( acc, r ) ->
                    if acc < divisor then
                        ( acc * 10 + el, r )

                    else
                        ( acc, el :: r )
                )
                ( 0, [] )
                dividend
    in
    if n < divisor then
        Nothing

    else
        Just ( Substraction.correctResult n (n // divisor * divisor), List.reverse rest )


emptyDivision : Division
emptyDivision =
    { dividend = 0
    , divisor = 0
    , result = 0
    , remainderRows = []
    }
