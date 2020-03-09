module Multiplication exposing (main)

import Browser
import Html
import Random


type alias Model =
    { currentOperation : Multiplication
    , minNumOfDigits : Int
    , maxNumOfDigits : Int
    }


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


type Msg
    = Msg


emptyModel : Model
emptyModel =
    { currentOperation = emptyMultiplication
    , minNumOfDigits = 1
    , maxNumOfDigits = 2
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


multNumGenerator : Int -> Int -> Random.Generator Int
multNumGenerator min max =
    Random.Int min max


correctResult : Int -> Int -> Multiplication
correctResult n1 n2 =
    -- TODO the algorithm
    { emptyMultiplication | multiplicand = n1, multiplier = n2 }


type CheckedDigit
    = IsOk Int
    | IsWrong Int Int


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


checkResult : Multiplication -> AnnotatedMultiplication
checkResult m =
    -- TODO the actual checking
    { emptyAnnotatedMultiplication | multiplicand = m.multiplicand, multiplier = m.multiplier }


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( emptyModel, Cmd.none )
        , view = \_ -> Html.div [] []
        , update = \_ m -> ( m, Cmd.none )
        , subscriptions = \_ -> Sub.none
        }
