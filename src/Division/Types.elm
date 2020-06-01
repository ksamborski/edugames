module Division.Types exposing
    ( DivisionInput
    , Model
    , Msg(..)
    , RemainderRowInput
    , emptyDivisionInput
    , emptyModel
    )

import Array exposing (Array)


type alias Model =
    { currentOperation : DivisionInput
    }


type alias DivisionInput =
    { dividend : Int
    , divisor : Int
    , resultRow : Array (Maybe Int)
    , remainderRows : List RemainderRowInput
    }


type alias RemainderRowInput =
    { upperRows : List (Array (Maybe Int))
    , resultRows : List (Array (Maybe Int))
    }


type Msg
    = Msg
    | ChangeResult Int (Maybe Int)


emptyModel : Model
emptyModel =
    { currentOperation = emptyDivisionInput
    }


emptyDivisionInput : DivisionInput
emptyDivisionInput =
    { dividend = 0
    , divisor = 0
    , resultRow = Array.repeat 5 Nothing
    , remainderRows = []
    }
