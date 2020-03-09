module Multiplication exposing (main)

import Browser
import Html


type alias Model =
    {}


type Msg
    = Msg


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( {}, Cmd.none )
        , view = \_ -> Html.div [] []
        , update = \_ m -> ( m, Cmd.none )
        , subscriptions = \_ -> Sub.none
        }
