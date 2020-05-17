module Math.Substraction exposing (Substraction, correctResult, emptySubstraction)

import Array exposing (Array)
import Math.Utils as Math
import Maybe.Extra as Maybe


type alias UpperRows =
    ( Array (Maybe Int), Array (Maybe Int) )


type alias UpperRow =
    Array (Maybe Int)


type alias Substraction =
    { minuend : Int
    , subtrahend : Int
    , upperRow1 : UpperRow
    , upperRow2 : UpperRow
    , result : Int
    }


correctResult : Int -> Int -> Substraction
correctResult minuend subtrahend =
    let
        ( row1, row2 ) =
            runSteps (Math.decimals minuend) (Math.decimals subtrahend)
    in
    { minuend = minuend
    , subtrahend = subtrahend
    , upperRow1 = row1
    , upperRow2 = row2
    , result = minuend - subtrahend
    }


runSteps : List Int -> List Int -> UpperRows
runSteps minuend subtrahend =
    let
        ( m, s ) =
            Math.equalLenList True 0 minuend subtrahend

        initialRow =
            Array.repeat (List.length m) Nothing
    in
    runStepsRecursively
        (List.reverse m)
        (List.reverse s)
        (List.length m - 1)
        ( initialRow, initialRow )


runStepsRecursively : List Int -> List Int -> Int -> UpperRows -> UpperRows
runStepsRecursively minuend subtrahend idx upperRows =
    if idx < 0 then
        upperRows

    else
        runStepsRecursively
            (List.drop 1 minuend)
            (List.drop 1 subtrahend)
            (idx - 1)
        <|
            runStep
                (Maybe.withDefault 0 <| List.head minuend)
                (Maybe.withDefault 0 <| List.head subtrahend)
                idx
                upperRows


runStep : Int -> Int -> Int -> UpperRows -> UpperRows
runStep minuend subtrahend idx upperRows =
    let
        ( n, upperRows2, needAnotherTen ) =
            recalculateOverwrites minuend idx upperRows

        updateNext rows =
            if needAnotherTen then
                overwrite (idx - 1) -1 rows

            else
                rows
    in
    if n < subtrahend then
        overwrite (idx - 1) -1 <|
            overwrite idx (n + 10) <|
                updateNext upperRows2

    else
        updateNext upperRows2


recalculateOverwrites : Int -> Int -> UpperRows -> ( Int, UpperRows, Bool )
recalculateOverwrites minuend idx ( row1, row2 ) =
    let
        ( n1, upRow1, needAnotherTen1 ) =
            recalculateOverwrite minuend idx row1

        ( n2, upRow2, needAnotherTen2 ) =
            recalculateOverwrite n1 idx row2
    in
    ( n2, ( upRow1, upRow2 ), needAnotherTen1 || needAnotherTen2 )


recalculateOverwrite : Int -> Int -> UpperRow -> ( Int, UpperRow, Bool )
recalculateOverwrite n idx row =
    case Maybe.join <| Array.get idx row of
        Just x ->
            if x < 0 then
                if n + x >= 0 then
                    ( n + x, Array.set idx (Just <| n + x) row, False )

                else
                    ( 10 + n + x, Array.set idx (Just <| n + x + 10) row, True )

            else
                ( x, row, False )

        Nothing ->
            ( n, row, False )


overwrite : Int -> Int -> UpperRows -> UpperRows
overwrite idx val ( row1, row2 ) =
    case Maybe.join <| Array.get idx row1 of
        Just _ ->
            ( row1, Array.set idx (Just val) row2 )

        Nothing ->
            ( Array.set idx (Just val) row1, row2 )


emptySubstraction : Substraction
emptySubstraction =
    { minuend = 0
    , subtrahend = 0
    , upperRow1 = Array.repeat 1 Nothing
    , upperRow2 = Array.repeat 1 Nothing
    , result = 0
    }
