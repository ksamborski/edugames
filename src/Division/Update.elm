module Division.Update exposing (update)

import Division.Types as Division


update : Division.Msg -> Division.Model -> ( Division.Model, Cmd Division.Msg )
update msg m =
    ( m, Cmd.none )
