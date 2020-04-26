module Math.Multiplication exposing
    ( AnnotatedMultiplication
    , CheckedDigit(..)
    , Multiplication
    , correctResult
    , decimals
    , emptyAnnotatedMultiplication
    , emptyMultiplication
    , errors
    , fixedRows
    , isOk
    )

import List.Extra exposing (cartesianProduct, dropWhile, getAt, groupsOf, interweave, last, lift2, setAt, transpose, zip)


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


emptyMultiplication : Multiplication
emptyMultiplication =
    { multiplicand = 0
    , multiplier = 0
    , resultRows = []
    , upperRows = []
    , sumUpperRow = []
    , finalResult = []
    }


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
    { result | finalResult = trFinalResult, sumUpperRow = result.sumUpperRow ++ [ 0 ] }


runSumStep : List Int -> ( Multiplication, Int ) -> ( Multiplication, Int )
runSumStep nums ( acc, rest ) =
    let
        s =
            List.sum nums
                + rest

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
                | sumUpperRow = rmd :: acc.sumUpperRow
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
                , upperRow = [ 0 ]
                , rest = 0
                }
                (multiplicationSteps n1 n2)
    in
    { multStage
        | resultRows = List.reverse multStage.resultRows
        , upperRows = List.reverse multStage.upperRows
    }


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
            , upperRow = [ 0 ]
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


equalLenList : Bool -> a -> List a -> List a -> ( List a, List a )
equalLenList prepend filler lst1 lst2 =
    let
        lst1len =
            List.length lst1

        lst2len =
            List.length lst2

        merge =
            if prepend then
                \a b -> a ++ b

            else
                \a b -> b ++ a
    in
    if lst1len > lst2len then
        ( lst1, merge (List.repeat (lst1len - lst2len) filler) lst2 )

    else
        ( merge (List.repeat (lst2len - lst1len) filler) lst1, lst2 )


nonzero : List number -> List number
nonzero =
    List.filter ((/=) 0)


maxColLen : List (List a) -> Int
maxColLen lst =
    Maybe.withDefault 1 <| List.maximum <| List.map List.length lst


errors : Bool -> Multiplication -> Maybe AnnotatedMultiplication
errors checkUpperrows m =
    let
        correct =
            correctResult m.multiplicand m.multiplier

        diff =
            { emptyAnnotatedMultiplication
                | multiplicand = m.multiplicand
                , multiplier = m.multiplier
                , resultRows =
                    List.map (\( a, b ) -> diffList True a b) <| zip givenResRows wantedResRows
                , upperRows = transpose <| fixedRows False 1 (IsOk 0) upperCols
                , sumUpperRow =
                    if checkUpperrows then
                        diffList True m.sumUpperRow correct.sumUpperRow

                    else
                        diffList True m.sumUpperRow m.sumUpperRow
                , finalResult =
                    diffList True m.finalResult correct.finalResult
            }

        upperCols =
            List.map
                (\( a, b ) ->
                    let
                        ( fixedA, fixedB ) =
                            equalLenList False 0 (nonzero a) (nonzero b)
                    in
                    diffList False fixedA fixedB
                )
            <|
                zip
                    (transpose <| fixedRows True (maxColLen wantedUpRows) 0 givenUpRows)
                    (transpose <| fixedRows True (maxColLen givenUpRows) 0 wantedUpRows)

        ( givenResRows, wantedResRows ) =
            equalLenList False [] m.resultRows correct.resultRows

        ( givenUpRows, wantedUpRows ) =
            if checkUpperrows then
                equalLenList False [] m.upperRows correct.upperRows

            else
                ( m.upperRows, m.upperRows )

        ok =
            List.all identity
                [ List.all (List.all isOk) diff.resultRows
                , List.all (List.all isOk) diff.upperRows
                , List.all isOk diff.sumUpperRow
                , List.all isOk diff.finalResult
                ]
    in
    if ok then
        Nothing

    else
        Just diff


fixedRows : Bool -> Int -> a -> List (List a) -> List (List a)
fixedRows prepend min filler lst =
    let
        upto =
            max min <|
                Maybe.withDefault min <|
                    List.maximum <|
                        List.map List.length lst
    in
    if prepend then
        List.map (\l -> List.repeat (upto - List.length l) filler ++ l) lst

    else
        List.map (\l -> l ++ List.repeat (upto - List.length l) filler) lst


isOk : CheckedDigit -> Bool
isOk d =
    case d of
        IsOk _ ->
            True

        _ ->
            False


diffList : Bool -> List Int -> List Int -> List CheckedDigit
diffList prepend given wanted =
    equalLenList prepend 0 given wanted
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
