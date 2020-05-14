module Math.Division exposing (Division, Substraction)


type alias Division =
    { dividend : Int
    , divisor : Int
    , resultRow : List Int
    , remainderRows : List Substraction
    }


type alias Substraction =
    { minuend : List Int
    , subtrahend : List Int
    , upperRows : ( List Int, List Int )
    , resultRow : List Int
    }
