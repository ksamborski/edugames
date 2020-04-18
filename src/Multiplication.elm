module Multiplication exposing (main)

import Animator
import Browser
import Element
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes as Html
import Multiplication.Types as Multiplication
import Multiplication.View as Multiplication
import Random
import Time exposing (Posix)


type alias Model =
    { game : Multiplication.Model
    , minNumOfDigits : Int
    , maxNumOfDigits : Int
    }


type Msg
    = NewMultiplication ( Int, Int )
    | AnimationTick Posix
    | GameMsg Multiplication.Msg


emptyModel : Model
emptyModel =
    { game = Multiplication.emptyModel
    , minNumOfDigits = 1
    , maxNumOfDigits = 3
    }


multNumGenerator : Int -> Int -> Random.Generator Int
multNumGenerator digMin digMax =
    let
        numMin =
            if digMin <= 1 then
                0

            else
                10 ^ (digMin - 1)

        numMax =
            if digMax <= 1 then
                0

            else
                10 ^ digMax - 1
    in
    Random.int numMin numMax


main : Program () Model Msg
main =
    Browser.element
        { init =
            \_ ->
                ( emptyModel
                , Random.generate
                    NewMultiplication
                    (Random.pair
                        (multNumGenerator emptyModel.minNumOfDigits emptyModel.maxNumOfDigits)
                        (multNumGenerator emptyModel.minNumOfDigits emptyModel.maxNumOfDigits)
                    )
                )
        , view = view
        , update = update
        , subscriptions = \m -> Animator.toSubscription AnimationTick m animator
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg m =
    case msg of
        NewMultiplication ( n, n2 ) ->
            let
                ( gv, cmds ) =
                    Multiplication.init n n2
            in
            ( { m | game = gv }, Cmd.map GameMsg cmds )

        AnimationTick newTime ->
            ( Animator.update newTime animator m, Cmd.none )

        GameMsg gmsg ->
            let
                ( gv, cmds ) =
                    Multiplication.update gmsg m.game
            in
            ( { m | game = gv }, Cmd.map GameMsg cmds )


animator : Animator.Animator Model
animator =
    Multiplication.setUpAnimator .game (\m g -> { m | game = g }) Animator.animator


view : Model -> Html Msg
view m =
    Element.layout
        [ Background.color (Element.rgb255 220 220 220)
        , Element.htmlAttribute <|
            Html.style "background-image" "linear-gradient(rgba(255,255,255,.3) 1px, transparent 1px), linear-gradient(90deg, rgba(255,255,255,.3) 1px, transparent 1px)"
        , Element.htmlAttribute <|
            Html.style "background-size" "20px 20px, 20px 20px, 20px 20px, 20px 20px"
        , Element.htmlAttribute <|
            Html.style "background-position" "-2px -2px, -2px -2px, -1px -1px, -1px -1px"
        ]
    <|
        Element.row [ Element.spacing 40 ]
            [ Element.map GameMsg <| Multiplication.calculationView m.game
            , operationView m
            ]


operationView : Model -> Element.Element Msg
operationView m =
    Input.button
        [ Background.color (Element.rgb255 200 200 200)
        , Element.height (Element.px 40)
        , Element.width (Element.px 160)
        , Element.focused []
        , Element.mouseOver [ Background.color (Element.rgb255 230 230 230) ]
        , Font.center
        , Border.width 1
        , Border.solid
        , Border.color (Element.rgb 1 1 1)
        ]
        { onPress = Just (GameMsg Multiplication.CheckResult)
        , label = Element.text "Sprawdź"
        }
