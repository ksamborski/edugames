module Multiplication exposing (main)

import Animator
import Browser
import Browser.Dom as Dom
import Browser.Events as Events
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
import Task
import Time exposing (Posix)


type alias Model =
    { game : Multiplication.Model
    , minNumOfDigits : Int
    , maxNumOfDigits : Int
    , maxNumOfRetries : Int
    , numOfOperations : Int
    , currentOperation : Maybe Operation
    , pendingOperations : List Operation
    , doneOperations : List Operation
    , page : Page
    , width : Int
    , height : Int
    }


type Page
    = SettingsPage
    | GamePage
    | GameFinishedPage


type alias Operation =
    { timeStart : Posix
    , timeEnd : Posix
    , multiplicand : Int
    , multiplier : Int
    , retries : Int
    , passed : Bool
    }


type Msg
    = NewMultiplication (List ( Int, Int ))
    | AnimationTick Posix
    | GameMsg Multiplication.Msg
    | ChangeMinNumOfDigits Int
    | ChangeMaxNumOfDigits Int
    | ChangeMaxNumOfRetries Int
    | ChangeNumOfOperations Int
    | Start
    | StartOperation Time.Posix
    | SkipOperation
    | EndOperation Time.Posix
    | NewGame
    | GotSize Dom.Element
    | Resized
    | NoOp


emptyOperation : Operation
emptyOperation =
    { timeStart = Time.millisToPosix 0
    , timeEnd = Time.millisToPosix 0
    , multiplicand = 0
    , multiplier = 0
    , retries = 0
    , passed = False
    }


emptyModel : Model
emptyModel =
    { game = Multiplication.emptyModel
    , minNumOfDigits = 1
    , maxNumOfDigits = 3
    , maxNumOfRetries = 1
    , numOfOperations = 10
    , currentOperation = Nothing
    , pendingOperations = []
    , doneOperations = []
    , page = SettingsPage
    , width = 0
    , height = 0
    }


multNumGenerator : Int -> Int -> Random.Generator ( Int, Int )
multNumGenerator digMin digMax =
    let
        numMin =
            if digMax <= 1 then
                0

            else
                10 ^ (digMax - 1)

        numMax =
            if digMax < 1 then
                0

            else
                10 ^ digMax - 1
    in
    Random.pair (Random.int digMin digMax) (Random.int numMin numMax)


multNumPairsGenerator : Int -> Int -> Int -> Random.Generator (List ( Int, Int ))
multNumPairsGenerator n digMin digMax =
    Random.list
        n
    <|
        Random.map2
            (\( d1, n1 ) ( d2, n2 ) -> ( remainderBy (10 ^ d1) n1, remainderBy (10 ^ d2) n2 ))
            (multNumGenerator digMin digMax)
            (multNumGenerator digMin digMax)


main : Program () Model Msg
main =
    Browser.element
        { init =
            \_ ->
                ( emptyModel
                , Task.attempt
                    (Result.withDefault NoOp << Result.map GotSize)
                    (Dom.getElement "multiplication")
                )
        , view = view
        , update = update
        , subscriptions =
            \m ->
                Sub.batch
                    [ Animator.toSubscription AnimationTick m animator
                    , Events.onResize (\_ _ -> Resized)
                    ]
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg m =
    case msg of
        NewMultiplication lst ->
            let
                ops =
                    List.map
                        (\( n1, n2 ) ->
                            { emptyOperation | multiplicand = max n1 n2, multiplier = min n1 n2 }
                        )
                        lst
            in
            ( { m
                | pendingOperations = Maybe.withDefault [] <| List.tail ops
                , currentOperation = List.head ops
                , doneOperations = []
              }
            , Task.attempt (StartOperation << Result.withDefault (Time.millisToPosix 0)) Time.now
            )

        StartOperation time ->
            let
                op =
                    Maybe.withDefault emptyOperation m.currentOperation

                ( g, cmds ) =
                    Multiplication.init op.multiplicand op.multiplier
            in
            ( { m
                | currentOperation = Just { op | timeStart = time }
                , page = GamePage
                , game = g
              }
            , Cmd.batch
                [ Cmd.map GameMsg cmds
                , Task.attempt
                    (Result.withDefault NoOp << Result.map GotSize)
                    (Dom.getElement "multiplication")
                ]
            )

        SkipOperation ->
            ( m, Task.attempt (EndOperation << Result.withDefault (Time.millisToPosix 0)) Time.now )

        EndOperation time ->
            let
                op =
                    Maybe.withDefault emptyOperation m.currentOperation
            in
            case List.head m.pendingOperations of
                Just pending ->
                    ( { m
                        | doneOperations = { op | timeEnd = time, passed = m.game.passed } :: m.doneOperations
                        , currentOperation = List.head m.pendingOperations
                        , pendingOperations = Maybe.withDefault [] <| List.tail m.pendingOperations
                      }
                    , Task.attempt (StartOperation << Result.withDefault (Time.millisToPosix 0)) Time.now
                    )

                Nothing ->
                    ( { m
                        | doneOperations = { op | timeEnd = time, passed = m.game.passed } :: m.doneOperations
                        , currentOperation = Nothing
                        , pendingOperations = []
                        , page = GameFinishedPage
                      }
                    , Cmd.none
                    )

        AnimationTick newTime ->
            ( Animator.update newTime animator m, Cmd.none )

        GameMsg Multiplication.CheckResult ->
            let
                op =
                    Maybe.withDefault emptyOperation m.currentOperation
            in
            if op.retries >= m.maxNumOfRetries then
                ( m, Cmd.none )

            else
                let
                    ( g, cmds ) =
                        Multiplication.update Multiplication.CheckResult m.game
                in
                ( { m | game = g, currentOperation = Just { op | retries = min (op.retries + 1) m.maxNumOfRetries } }
                , Cmd.batch
                    [ Cmd.map GameMsg cmds
                    , if g.passed then
                        Task.attempt (EndOperation << Result.withDefault (Time.millisToPosix 0)) Time.now

                      else
                        Cmd.none
                    ]
                )

        GameMsg gmsg ->
            let
                ( g, cmds ) =
                    Multiplication.update gmsg m.game
            in
            ( { m | game = g }, Cmd.map GameMsg cmds )

        ChangeMinNumOfDigits n ->
            ( { m | minNumOfDigits = n }, Cmd.none )

        ChangeMaxNumOfDigits n ->
            ( { m | maxNumOfDigits = n }, Cmd.none )

        ChangeMaxNumOfRetries n ->
            ( { m | maxNumOfRetries = n }, Cmd.none )

        ChangeNumOfOperations n ->
            ( { m | numOfOperations = n }, Cmd.none )

        Start ->
            ( m
            , Random.generate
                NewMultiplication
                (multNumPairsGenerator m.numOfOperations m.minNumOfDigits m.maxNumOfDigits)
            )

        NewGame ->
            ( { m | currentOperation = Nothing, doneOperations = [], pendingOperations = [], page = SettingsPage }
            , Cmd.none
            )

        GotSize el ->
            ( { m | width = floor el.element.width, height = floor el.element.height }, Cmd.none )

        Resized ->
            ( { m | height = 0, width = 0 }
            , Task.attempt
                (Result.withDefault NoOp << Result.map GotSize)
                (Dom.getElement "multiplication")
            )

        NoOp ->
            ( m, Cmd.none )


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
        case m.page of
            SettingsPage ->
                settingsPageView m

            GamePage ->
                gamePageView m

            GameFinishedPage ->
                gameFinishedPageView m


frame : List (Element.Element Msg) -> Element.Element Msg
frame =
    Element.el
        [ Element.centerX
        , Element.centerY
        , Background.color (Element.rgba 1 1 1 0.75)
        , Border.widthXY 1 2
        , Border.color (Element.rgb255 10 10 200)
        , Border.rounded 5
        , Border.shadow
            { offset = ( 0, 0 )
            , size = 1
            , blur = 25
            , color = Element.rgb255 0 0 0
            }
        , Element.width (Element.px 500)
        ]
        << Element.column
            [ Element.height Element.fill
            , Element.width Element.fill
            , Element.spacing 20
            , Element.padding 20
            ]


settingsPageView : Model -> Element.Element Msg
settingsPageView m =
    frame
        [ header "Mnożenie pisemne"
        , numberSettingsInput "Minimalna liczba cyfr:" ( 1, 5 ) m.minNumOfDigits ChangeMinNumOfDigits
        , numberSettingsInput "Maksymalna liczba cyfr:" ( m.minNumOfDigits, 5 ) m.maxNumOfDigits ChangeMaxNumOfDigits
        , numberSettingsInput "Maksymalna liczba prób:" ( 1, 10 ) m.maxNumOfRetries ChangeMaxNumOfRetries
        , numberSettingsInput "Liczba działań:" ( 1, 100 ) m.numOfOperations ChangeNumOfOperations
        , frameButton "Start" Start
        ]


header : String -> Element.Element Msg
header =
    Element.el
        [ Font.center
        , Font.size 26
        , Font.family [ Font.typeface "Oswald", Font.sansSerif ]
        , Element.width Element.fill
        , Element.paddingEach { top = 0, right = 0, left = 0, bottom = 15 }
        ]
        << Element.text


numberSettingsInput : String -> ( Int, Int ) -> Int -> (Int -> Msg) -> Element.Element Msg
numberSettingsInput label ( minN, maxN ) value action =
    Input.text
        [ Element.htmlAttribute (Html.type_ "number")
        , Element.htmlAttribute (Html.min <| String.fromInt minN)
        , Element.htmlAttribute (Html.max <| String.fromInt maxN)
        , Element.width (Element.px 100)
        ]
        { onChange = action << Maybe.withDefault value << String.toInt
        , text = String.fromInt value
        , placeholder = Nothing
        , label =
            Input.labelLeft [ Element.height Element.fill, Element.width Element.fill, Element.paddingXY 5 0 ] <|
                Element.el [ Element.centerY, Font.size 20, Font.family [ Font.typeface "Slabo 27px", Font.serif ] ] <|
                    Element.text label
        }


frameButton : String -> Msg -> Element.Element Msg
frameButton label onpress =
    Input.button
        [ Background.color (Element.rgb255 200 200 200)
        , Element.height (Element.px 40)
        , Element.width Element.fill
        , Element.focused []
        , Element.mouseOver [ Background.color (Element.rgb255 230 230 230) ]
        , Font.center
        , Border.width 1
        , Border.solid
        , Border.color (Element.rgb 1 1 1)
        ]
        { onPress = Just onpress
        , label = Element.text label
        }


frameLine : String -> ( String, Element.Color ) -> Element.Element Msg
frameLine label ( value, valueColor ) =
    Element.row [ Element.width Element.fill, Element.spacing 5 ]
        [ Element.el [ Element.centerY, Font.size 20, Font.family [ Font.typeface "Slabo 27px", Font.serif ] ] <|
            Element.text label
        , Element.el [ Font.color valueColor, Element.alignRight, Element.centerY, Font.size 20, Font.family [ Font.typeface "Slabo 27px", Font.serif ] ] <|
            Element.text value
        ]


gameFinishedPageView : Model -> Element.Element Msg
gameFinishedPageView m =
    let
        { passed, retries, minTime, maxTime } =
            List.foldl
                (\op acc ->
                    { acc
                        | passed =
                            if op.passed then
                                acc.passed + 1

                            else
                                acc.passed
                        , retries = op.retries + acc.retries
                        , minTime =
                            if acc.minTime == 0 then
                                Time.posixToMillis op.timeStart

                            else
                                min (Time.posixToMillis op.timeStart) acc.minTime
                        , maxTime = max acc.maxTime (Time.posixToMillis op.timeEnd)
                    }
                )
                { passed = 0, retries = 0, minTime = 0, maxTime = 0 }
                m.doneOperations

        numFrom =
            if m.minNumOfDigits <= 1 then
                0

            else
                10 ^ (m.minNumOfDigits - 1)

        numTo =
            10 ^ m.maxNumOfDigits - 1
    in
    frame
        [ header "Koniec gry"
        , frameLine "Przedział liczbowy:" ( "od " ++ String.fromInt numFrom ++ " do " ++ String.fromInt numTo, Element.rgb 0 0 0 )
        , frameLine "Liczba działań:" ( String.fromInt m.numOfOperations, Element.rgb 0 0 0 )
        , frameLine "Liczba poprawnych odpowiedzi:" ( String.fromInt passed, Element.rgb 0 1 0 )
        , frameLine "Liczba błędnych odpowiedzi:" ( String.fromInt (m.numOfOperations - passed), Element.rgb 1 0 0 )
        , frameLine "Liczba prób:" ( String.fromInt retries, Element.rgb 0 0 0 )
        , frameLine "Czas całkowity:" ( timeString minTime maxTime, Element.rgb 0 0 0 )
        , frameButton "Nowa gra" NewGame
        ]


timeString : Int -> Int -> String
timeString from to =
    let
        nsec =
            (to - from) // 1000

        hours =
            nsec // 3600

        minutes =
            (nsec - (hours * 3600)) // 60

        sec =
            nsec - (hours * 3600) - (minutes * 60)
    in
    (if hours < 10 then
        "0"

     else
        ""
    )
        ++ String.fromInt hours
        ++ ":"
        ++ (if minutes < 10 then
                "0"

            else
                ""
           )
        ++ String.fromInt minutes
        ++ ":"
        ++ (if sec < 10 then
                "0"

            else
                ""
           )
        ++ String.fromInt sec


gamePageView : Model -> Element.Element Msg
gamePageView m =
    let
        op =
            Maybe.withDefault emptyOperation m.currentOperation
    in
    Element.row
        [ Element.height Element.fill
        , Element.width Element.fill
        ]
        [ operationView m
        , Element.column
            [ Element.height Element.fill
            , Element.width Element.fill
            , Element.htmlAttribute <| Html.id "multiplication"
            ]
            [ if op.retries >= m.maxNumOfRetries then
                Element.row [ Element.alignTop, Element.width Element.fill, Element.paddingXY 0 19 ]
                    [ withHeader "Twoja odpowiedź" (Element.rgb 1 0 0) <|
                        Element.map GameMsg <|
                            Multiplication.calculationView m.game (m.width // 2)
                    , Element.el [ Element.centerX, Border.width 2, Border.solid, Element.height Element.fill ] Element.none
                    , withHeader "Poprawna odpowiedź" (Element.rgb 0 1 0) <|
                        Element.map GameMsg <|
                            Multiplication.correctCalculationView m.game (m.width // 2)
                    ]

              else
                Element.map GameMsg <| Multiplication.calculationView m.game m.width
            , arrowKeysView
            ]
        ]


withHeader : String -> Element.Color -> Element.Element Msg -> Element.Element Msg
withHeader txt clr el =
    Element.column
        [ Element.alignTop, Element.width Element.fill ]
        [ Element.el
            [ Font.center
            , Font.size 26
            , Font.family [ Font.typeface "Oswald", Font.sansSerif ]
            , Element.width Element.fill
            , Element.paddingEach { right = 0, left = 0, bottom = 17, top = 17 }
            , Font.color clr
            , Element.alignTop
            ]
            (Element.text txt)
        , el
        ]


arrowKeysView : Element.Element Msg
arrowKeysView =
    Element.column [ Element.spacing 10, Element.padding 10, Element.alignRight, Element.alignBottom ]
        [ arrowKeyButton "🡅" (GameMsg <| Multiplication.Focus Multiplication.FocusUp)
        , Element.row [ Element.width Element.fill, Element.spacing 60 ]
            [ arrowKeyButton "🡄" (GameMsg <| Multiplication.Focus Multiplication.FocusLeft)
            , arrowKeyButton "🡆" (GameMsg <| Multiplication.Focus Multiplication.FocusRight)
            ]
        , arrowKeyButton "🡇" (GameMsg <| Multiplication.Focus Multiplication.FocusDown)
        , arrowKeysViewHint "Enter wywołuje sprawdzenie"
        , arrowKeysViewHint "Używaj strzałek by się poruszać"
        ]


arrowKeyButton : String -> Msg -> Element.Element Msg
arrowKeyButton label action =
    Input.button
        [ Background.color (Element.rgb255 200 200 200)
        , Element.height (Element.px 40)
        , Element.width (Element.px 40)
        , Element.focused []
        , Element.mouseOver [ Background.color (Element.rgb255 230 230 230) ]
        , Font.center
        , Border.width 1
        , Border.solid
        , Border.color (Element.rgb 1 1 1)
        , Element.centerX
        ]
        { onPress = Just action
        , label = Element.text label
        }


arrowKeysViewHint : String -> Element.Element Msg
arrowKeysViewHint =
    Element.el
        [ Element.width Element.fill
        , Font.alignRight
        , Font.size 20
        , Font.family [ Font.typeface "Slabo 27px", Font.serif ]
        ]
        << Element.text


operationViewWidth : Int
operationViewWidth =
    259


operationView : Model -> Element.Element Msg
operationView m =
    let
        pending =
            List.length m.pendingOperations

        done =
            List.length m.doneOperations

        cop =
            Maybe.withDefault emptyOperation m.currentOperation
    in
    Element.column
        [ Element.spacing 20
        , Element.padding 10
        , Background.color (Element.rgba 1 1 1 0.75)
        , Border.widthEach { top = 0, bottom = 0, left = 0, right = 1 }
        , Border.color (Element.rgb255 10 10 200)
        , Element.width (Element.px operationViewWidth)
        , Element.height Element.fill
        ]
        [ header <|
            "Działanie "
                ++ String.fromInt (done + 1)
                ++ " / "
                ++ String.fromInt (pending + done + 1)
        , operationLine <| "Pozostało prób: " ++ String.fromInt (m.maxNumOfRetries - cop.retries)
        , if m.maxNumOfRetries - cop.retries > 0 then
            operationButton "Sprawdź" (GameMsg Multiplication.CheckResult) (Element.rgb255 200 200 200)

          else
            Element.none
        , operationButton "Pomiń" SkipOperation (Element.rgb255 250 90 90)
        ]


operationButton : String -> Msg -> Element.Color -> Element.Element Msg
operationButton label action clr =
    Input.button
        [ Background.color clr
        , Element.height (Element.px 40)
        , Element.width (Element.px 160)
        , Element.focused []
        , Element.mouseOver [ Background.color (Element.rgb255 230 230 230) ]
        , Font.center
        , Border.width 1
        , Border.solid
        , Border.color (Element.rgb 1 1 1)
        , Element.centerX
        ]
        { onPress = Just action
        , label = Element.text label
        }


operationLine : String -> Element.Element Msg
operationLine =
    Element.el
        [ Element.centerX
        , Font.size 20
        , Font.family [ Font.typeface "Slabo 27px", Font.serif ]
        ]
        << Element.text
