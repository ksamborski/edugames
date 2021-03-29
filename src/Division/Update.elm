module Division.Update exposing (update)

import Array
import Division.Types as Division
import List.Extra as List


update : Division.Msg -> Division.Model -> ( Division.Model, Cmd Division.Msg )
update msg m =
    case msg of
        Division.ChangeResult x mv ->
            ( { m | currentOperation = changeResult m.currentOperation x mv }, Cmd.none )

        Division.ChangeRemainderResult rIdx y x mv ->
            ( { m | currentOperation = changeRemainderResult m.currentOperation rIdx y x mv }, Cmd.none )


changeResult : Division.DivisionInput -> Int -> Maybe Int -> Division.DivisionInput
changeResult cop x mv =
    { cop | resultRow = Array.set x mv cop.resultRow }


changeRemainderResult : Division.DivisionInput -> Int -> Int -> Int -> Maybe Int -> Division.DivisionInput
changeRemainderResult cop rIdx y x mv =
    { cop
        | remainderRows =
            if rIdx >= List.length cop.remainderRows then
                changeRemainderResultRow cop.rowLen y x mv Division.emptyRemainderRowInput
                    :: cop.remainderRows

            else
                List.updateAt rIdx (changeRemainderResultRow cop.rowLen y x mv) cop.remainderRows
    }


changeRemainderResultRow : Int -> Int -> Int -> Maybe Int -> Division.RemainderRowInput -> Division.RemainderRowInput
changeRemainderResultRow len y x mv rowInput =
    { rowInput
        | resultRows =
            if y >= List.length rowInput.resultRows then
                Array.set x mv (Array.repeat len Nothing)
                    :: rowInput.resultRows

            else
                List.updateAt y (Array.set x mv) rowInput.resultRows
    }
