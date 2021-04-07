module Division.Update exposing (update)

import Array
import Division.Types as Division
import List.Extra as List


type alias RemainderUpdateF =
    Int -> Int -> Int -> Maybe Int -> Division.RemainderRowInput -> Division.RemainderRowInput


update : Division.Msg -> Division.Model -> ( Division.Model, Cmd Division.Msg )
update msg m =
    case msg of
        Division.ChangeResult x mv ->
            ( { m | currentOperation = changeResult m.currentOperation x mv }, Cmd.none )

        Division.ChangeRemainderResult rIdx y x mv ->
            ( { m | currentOperation = changeRemainder changeRemainderResultRow m.currentOperation rIdx y x mv }, Cmd.none )

        Division.ChangeRemainderUpper rIdx y x mv ->
            ( { m | currentOperation = changeRemainder changeRemainderUpperRow m.currentOperation rIdx y x mv }, Cmd.none )


changeResult : Division.DivisionInput -> Int -> Maybe Int -> Division.DivisionInput
changeResult cop x mv =
    { cop | resultRow = Array.set x mv cop.resultRow }


changeRemainder : RemainderUpdateF -> Division.DivisionInput -> Int -> Int -> Int -> Maybe Int -> Division.DivisionInput
changeRemainder f cop rIdx y x mv =
    { cop
        | remainderRows =
            if rIdx >= List.length cop.remainderRows then
                f cop.rowLen y x mv Division.emptyRemainderRowInput
                    :: cop.remainderRows

            else
                List.updateAt rIdx (f cop.rowLen y x mv) cop.remainderRows
    }


changeRemainderResultRow : Int -> Int -> Int -> Maybe Int -> Division.RemainderRowInput -> Division.RemainderRowInput
changeRemainderResultRow len y x mv rowInput =
    { rowInput
        | resultRows = updateRemainderRow True len y x mv rowInput.resultRows
    }


changeRemainderUpperRow : Int -> Int -> Int -> Maybe Int -> Division.RemainderRowInput -> Division.RemainderRowInput
changeRemainderUpperRow len y x mv rowInput =
    { rowInput
        | upperRows = updateRemainderRow False len y x mv rowInput.upperRows
    }


updateRemainderRow : Bool -> Int -> Int -> Int -> Maybe Int -> Division.RemainderRowInputRows -> Division.RemainderRowInputRows
updateRemainderRow prepend len y x mv rows =
    if y >= List.length rows then
        if prepend then
            Array.set x mv (Array.repeat len Nothing)
                :: rows

        else
            List.append
                rows
                [ Array.set x mv (Array.repeat len Nothing) ]

    else
        List.updateAt y (Array.set x mv) rows
