module Division exposing (main)

import Browser
import Browser.Dom as Dom
import Browser.Events as Events
import Division.Types as Division
import Division.View as Division
import Element
import Html exposing (Html)
import I18Next
import Json.Decode
import Json.Encode
import Task
import Theme.Math as Theme


type alias Model =
    { page : Page
    , width : Int
    , height : Int
    , translations : I18Next.Translations
    , game : Division.Model
    }


type Page
    = GamePage


type Msg
    = GotSize Dom.Element
    | Resized
    | NoOp
    | GameMsg Division.Msg


main : Program Json.Encode.Value Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Events.onResize (\_ _ -> Resized)
        }


init : Json.Encode.Value -> ( Model, Cmd Msg )
init flags =
    case Json.Decode.decodeValue I18Next.translationsDecoder flags of
        Ok translations ->
            ( { emptyModel | translations = translations }
            , Task.attempt
                (Result.withDefault NoOp << Result.map GotSize)
                (Dom.getElement "division")
            )

        Err _ ->
            ( emptyModel
            , Task.attempt
                (Result.withDefault NoOp << Result.map GotSize)
                (Dom.getElement "division")
            )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg m =
    case msg of
        GameMsg _ ->
            ( m, Cmd.none )

        GotSize el ->
            ( { m | width = floor el.element.width, height = floor el.element.height }, Cmd.none )

        Resized ->
            ( { m | height = 0, width = 0 }
            , Task.attempt
                (Result.withDefault NoOp << Result.map GotSize)
                (Dom.getElement "division")
            )

        NoOp ->
            ( m, Cmd.none )


view : Model -> Html Msg
view m =
    Element.layout
        Theme.gridBackground
        (gamePageView m)


gamePageView : Model -> Element.Element Msg
gamePageView m =
    Element.map GameMsg <| Division.calculationView m.game m.width


emptyModel : Model
emptyModel =
    { page = GamePage
    , width = 0
    , height = 0
    , translations = I18Next.initialTranslations
    , game = Division.emptyModel
    }
