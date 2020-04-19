module Multiplication.Types exposing
    ( ColId(..)
    , FocusDirection(..)
    , Focused(..)
    , Model
    , Msg(..)
    , MultiplicationInput
    , RowId(..)
    , changeFocus
    , colId
    , colId2focused
    , emptyModel
    , emptyMultiplicationInput
    , focused2ColId
    , input2multiplication
    , rowId
    )

import Animator
import Math.Multiplication exposing (..)


type alias Model =
    { currentOperation : MultiplicationInput
    , passed : Bool
    , errors : Animator.Timeline (Maybe AnnotatedMultiplication)
    , focused : Focused
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


type Focused
    = FocusedUpperRow Int Int
    | FocusedResultRow Int Int
    | FocusedSumUpperRow Int
    | FocusedFinalRow Int
    | FocusedNothing


type FocusDirection
    = FocusUp
    | FocusDown
    | FocusLeft
    | FocusRight


type Msg
    = UpperRowInput Int Int (Maybe Int)
    | ResultRowInput Int Int (Maybe Int)
    | SumUpperRowInput Int (Maybe Int)
    | FinalResultRowInput Int (Maybe Int)
    | CheckResult
    | Focus FocusDirection
    | ForceFocus ColId
    | NoOp


type RowId
    = UpperRow Int
    | SumUpperRow
    | ResultRow Int
    | FinalRow


type ColId
    = ColId RowId Int


rowId : RowId -> String
rowId rid =
    case rid of
        UpperRow i ->
            "upperrow" ++ String.fromInt i

        SumUpperRow ->
            "sumupperrow"

        ResultRow i ->
            "resultrow" ++ String.fromInt i

        FinalRow ->
            "finalrow"


colId : ColId -> String
colId (ColId rid i) =
    rowId rid ++ "," ++ String.fromInt i


focused2ColId : Focused -> ColId
focused2ColId f =
    case f of
        FocusedUpperRow x y ->
            ColId (UpperRow y) x

        FocusedResultRow x y ->
            ColId (ResultRow y) x

        FocusedSumUpperRow x ->
            ColId SumUpperRow x

        FocusedFinalRow x ->
            ColId FinalRow x

        FocusedNothing ->
            ColId (ResultRow 0) 0


colId2focused : ColId -> Focused
colId2focused (ColId rid x) =
    case rid of
        UpperRow y ->
            FocusedUpperRow x y

        SumUpperRow ->
            FocusedSumUpperRow x

        ResultRow y ->
            FocusedResultRow x y

        FinalRow ->
            FocusedFinalRow x


emptyModel : Model
emptyModel =
    { currentOperation = emptyMultiplicationInput
    , passed = False
    , errors = Animator.init Nothing
    , focused = FocusedNothing
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


changeFocus : MultiplicationInput -> Focused -> FocusDirection -> Focused
changeFocus op focused dir =
    let
        upperRowsLen =
            List.length op.upperRows + 1

        resultRowsLen =
            List.length op.resultRows + 1

        upperRowLen =
            1 + (List.length <| decimals op.multiplicand)

        rowLen =
            List.length (decimals op.multiplier) + List.length (decimals op.multiplicand)

        upRowDiff =
            rowLen - upperRowLen
    in
    case ( focused, dir ) of
        ( FocusedUpperRow x y, FocusUp ) ->
            if y < upperRowsLen - 1 then
                FocusedUpperRow x (y + 1)

            else
                FocusedUpperRow x (upperRowsLen - 1)

        ( FocusedUpperRow x y, FocusDown ) ->
            if y > 0 then
                FocusedUpperRow x (y - 1)

            else if resultRowsLen > 2 then
                FocusedSumUpperRow (x + upRowDiff)

            else
                FocusedResultRow (x + upRowDiff) 0

        ( FocusedUpperRow x y, FocusLeft ) ->
            if x > 0 then
                FocusedUpperRow (x - 1) y

            else
                FocusedUpperRow 0 y

        ( FocusedUpperRow x y, FocusRight ) ->
            if x < upperRowLen - 1 then
                FocusedUpperRow (x + 1) y

            else
                FocusedUpperRow (upperRowLen - 1) y

        ( FocusedResultRow x y, FocusUp ) ->
            if y > 0 then
                FocusedResultRow x (y - 1)

            else if resultRowsLen > 2 then
                FocusedSumUpperRow x

            else
                FocusedUpperRow (max 0 (min (x - upRowDiff) (upperRowLen - 1))) 0

        ( FocusedResultRow x y, FocusDown ) ->
            if y < resultRowsLen - 1 then
                FocusedResultRow x (y + 1)

            else if resultRowsLen > 2 then
                FocusedFinalRow x

            else
                FocusedResultRow x (resultRowsLen - 1)

        ( FocusedResultRow x y, FocusLeft ) ->
            if x > 0 then
                FocusedResultRow (x - 1) y

            else
                FocusedResultRow 0 y

        ( FocusedResultRow x y, FocusRight ) ->
            if x < rowLen - 1 then
                FocusedResultRow (x + 1) y

            else
                FocusedResultRow (rowLen - 1) y

        ( FocusedSumUpperRow x, FocusUp ) ->
            FocusedUpperRow (max 0 (min (x - upRowDiff) (upperRowLen - 1))) 0

        ( FocusedSumUpperRow x, FocusDown ) ->
            FocusedResultRow x 0

        ( FocusedSumUpperRow x, FocusLeft ) ->
            if x > 0 then
                FocusedSumUpperRow (x - 1)

            else
                FocusedSumUpperRow 0

        ( FocusedSumUpperRow x, FocusRight ) ->
            if x < rowLen - 1 then
                FocusedSumUpperRow (x + 1)

            else
                FocusedSumUpperRow (rowLen - 1)

        ( FocusedFinalRow x, FocusUp ) ->
            if resultRowsLen > 2 then
                FocusedResultRow x (resultRowsLen - 1)

            else
                FocusedUpperRow (max 0 (min (x - upRowDiff) (upperRowLen - 1))) 0

        ( FocusedFinalRow x, FocusDown ) ->
            FocusedFinalRow x

        ( FocusedFinalRow x, FocusLeft ) ->
            if x > 0 then
                FocusedFinalRow (x - 1)

            else
                FocusedFinalRow 0

        ( FocusedFinalRow x, FocusRight ) ->
            if x < rowLen - 1 then
                FocusedFinalRow (x + 1)

            else
                FocusedFinalRow (rowLen - 1)

        ( FocusedNothing, _ ) ->
            FocusedResultRow 0 (rowLen - 1)


input2multiplication : MultiplicationInput -> Multiplication
input2multiplication input =
    let
        resultLength =
            List.length input.resultRows

        fixMaybes =
            List.map (Maybe.withDefault 0)
    in
    { multiplicand = input.multiplicand
    , multiplier = input.multiplier
    , resultRows = List.map fixMaybes input.resultRows
    , upperRows = List.map fixMaybes input.upperRows
    , sumUpperRow = fixMaybes input.sumUpperRow
    , finalResult =
        fixMaybes <|
            if resultLength == 1 then
                Maybe.withDefault [] <| List.head input.resultRows

            else
                input.finalResult
    }
