module Division.Types exposing
    ( DivisionInput
    , Model
    , Msg(..)
    , RemainderRowInput
    , emptyDivisionInput
    , emptyModel
    , emptyRemainderRowInput
    )

import Array exposing (Array)


type alias Model =
    { currentOperation : DivisionInput
    }


type alias DivisionInput =
    { dividend : Int
    , divisor : Int
    , rowLen : Int
    , resultRow : Array (Maybe Int)
    , remainderRows : List RemainderRowInput
    }


type alias RemainderRowInput =
    { upperRows : List (Array (Maybe Int))
    , resultRows : List (Array (Maybe Int))
    }


type Msg
    = ChangeResult Int (Maybe Int)
    | ChangeRemainderResult Int Int Int (Maybe Int)


emptyModel : Model
emptyModel =
    { currentOperation = emptyDivisionInput
    }


emptyDivisionInput : DivisionInput
emptyDivisionInput =
    { dividend = 0
    , divisor = 0
    , rowLen = 5
    , resultRow = Array.repeat 5 Nothing
    , remainderRows = []
    }


emptyRemainderRowInput : RemainderRowInput
emptyRemainderRowInput =
    { upperRows = []
    , resultRows = []
    }
