module Division.Types exposing (Division, RemainderRowInput)


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
