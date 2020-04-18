module Multiplication exposing (CheckedDigit(..), Multiplication, correctResult, decimals, errors, main)

import Animator
import Browser
import Browser.Dom as Dom
import Element
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Element.Keyed as Keyed
import Html exposing (Html)
import Html.Attributes as Html
import Keyboard exposing (Key(..))
import Keyboard.Events as Keyboard
import List.Extra exposing (cartesianProduct, dropWhile, getAt, groupsOf, interweave, last, lift2, setAt, transpose, zip)
import Random
import Task
import Time exposing (Posix)


type alias Model =
    { currentOperation : MultiplicationInput
    , minNumOfDigits : Int
    , maxNumOfDigits : Int
    , checked : Bool
    , errors : Animator.Timeline (Maybe AnnotatedMultiplication)
    , focused : Focused
    }


type alias MultiplicationInput =
    { multiplicand : Int
    , multiplier : Int

    -- ^ user input
    , resultRows : List (List (Maybe Int))
    , upperRows : List (List (Maybe Int))

    -- ^ sums
    , sumUpperRow : List (Maybe Int)
    , finalResult : List (Maybe Int)
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


type Focused
    = FocusedUpperRow Int Int
    | FocusedResultRow Int Int
    | FocusedSumUpperRow Int
    | FocusedFinalRow Int
    | FocusedNothing


type FocusDirection
    = FocusUp
    | FocusDown
    | FocusLeft
    | FocusRight


type Step
    = Calculate ( Int, Int )
    | SaveRow Int


type Msg
    = NewMultiplication ( Int, Int )
    | UpperRowInput Int Int (Maybe Int)
    | ResultRowInput Int Int (Maybe Int)
    | SumUpperRowInput Int (Maybe Int)
    | FinalResultRowInput Int (Maybe Int)
    | CheckResult
    | AnimationTick Posix
    | Focus FocusDirection
    | ForceFocus ColId
    | NoOp


type RowId
    = UpperRow Int
    | SumUpperRow
    | ResultRow Int
    | FinalRow


type ColId
    = ColId RowId Int


rowId : RowId -> String
rowId rid =
    case rid of
        UpperRow i ->
            "upperrow" ++ String.fromInt i

        SumUpperRow ->
            "sumupperrow"

        ResultRow i ->
            "resultrow" ++ String.fromInt i

        FinalRow ->
            "finalrow"


colId : ColId -> String
colId (ColId rid i) =
    rowId rid ++ "," ++ String.fromInt i


focused2ColId : Focused -> ColId
focused2ColId f =
    case f of
        FocusedUpperRow x y ->
            ColId (UpperRow y) x

        FocusedResultRow x y ->
            ColId (ResultRow y) x

        FocusedSumUpperRow x ->
            ColId SumUpperRow x

        FocusedFinalRow x ->
            ColId FinalRow x

        FocusedNothing ->
            ColId (ResultRow 0) 0


colId2focused : ColId -> Focused
colId2focused (ColId rid x) =
    case rid of
        UpperRow y ->
            FocusedUpperRow x y

        SumUpperRow ->
            FocusedSumUpperRow x

        ResultRow y ->
            FocusedResultRow x y

        FinalRow ->
            FocusedFinalRow x


emptyModel : Model
emptyModel =
    { currentOperation = emptyMultiplicationInput
    , minNumOfDigits = 1
    , maxNumOfDigits = 2
    , checked = False
    , errors = Animator.init Nothing
    , focused = FocusedNothing
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


emptyMultiplicationInput : MultiplicationInput
emptyMultiplicationInput =
    { multiplicand = 0
    , multiplier = 0
    , resultRows = []
    , upperRows = []
    , sumUpperRow = []
    , finalResult = []
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


correctResult : Int -> Int -> Multiplication
correctResult n1 n2 =
    multiply n1 n2
        |> sumResults


sumResults : Multiplication -> Multiplication
sumResults m =
    let
        maxResultNumDig =
            Maybe.withDefault 0 <|
                List.maximum <|
                    List.map List.length m.resultRows

        resultsWithZeros =
            List.map
                (\row -> List.repeat (maxResultNumDig - List.length row) 0 ++ row)
                m.resultRows

        result =
            Tuple.first <|
                List.foldl
                    runSumStep
                    ( m, 0 )
                    ((List.reverse <| transpose resultsWithZeros) ++ [ [] ])

        trFinalResult =
            case dropWhile ((==) 0) result.finalResult of
                [] ->
                    [ 0 ]

                r ->
                    r
    in
    { result | finalResult = trFinalResult, sumUpperRow = result.sumUpperRow ++ [ 0 ] }


runSumStep : List Int -> ( Multiplication, Int ) -> ( Multiplication, Int )
runSumStep nums ( acc, rest ) =
    let
        s =
            List.sum nums
                + rest

        rmd =
            s // 10
    in
    case nums of
        [] ->
            if rest == 0 then
                ( acc, 0 )

            else
                ( { acc | finalResult = s :: acc.finalResult }, 0 )

        _ ->
            ( { acc
                | sumUpperRow = rmd :: acc.sumUpperRow
                , finalResult = remainderBy 10 s :: acc.finalResult
              }
            , rmd
            )


multiply : Int -> Int -> Multiplication
multiply n1 n2 =
    let
        { multStage } =
            List.foldl
                runMultiplicationStep
                { multStage = { emptyMultiplication | multiplicand = n1, multiplier = n2 }
                , resultRow = []
                , upperRow = [ 0 ]
                , rest = 0
                }
                (multiplicationSteps n1 n2)
    in
    { multStage
        | resultRows = List.reverse multStage.resultRows
        , upperRows = List.reverse multStage.upperRows
    }


runMultiplicationStep :
    Step
    -> { multStage : Multiplication, resultRow : List Int, upperRow : List Int, rest : Int }
    -> { multStage : Multiplication, resultRow : List Int, upperRow : List Int, rest : Int }
runMultiplicationStep op acc =
    case op of
        Calculate ( n, m ) ->
            let
                mult =
                    n * m + acc.rest

                newRest =
                    mult // 10
            in
            { multStage = acc.multStage
            , resultRow = remainderBy 10 mult :: acc.resultRow
            , upperRow = newRest :: acc.upperRow
            , rest = newRest
            }

        SaveRow zeros ->
            let
                mult =
                    acc.multStage

                resultSignDigits =
                    if acc.rest > 0 then
                        acc.rest :: acc.resultRow

                    else
                        acc.resultRow
            in
            { multStage =
                { mult
                    | resultRows = (resultSignDigits ++ List.repeat zeros 0) :: mult.resultRows
                    , upperRows = acc.upperRow :: mult.upperRows
                }
            , resultRow = []
            , upperRow = [ 0 ]
            , rest = 0
            }


multiplicationSteps : Int -> Int -> List Step
multiplicationSteps n1 n2 =
    let
        decN1 =
            List.reverse <| decimals n1

        decN2 =
            List.reverse <| decimals n2
    in
    cartesianProduct [ decN2, decN1 ]
        |> List.map
            (\input ->
                case input of
                    [ n, m ] ->
                        Calculate ( n, m )

                    _ ->
                        Calculate ( 0, 0 )
            )
        |> groupsOf (List.length decN1)
        |> (\l ->
                interweave l (List.indexedMap (\i f -> [ f i ]) <| List.repeat (List.length decN2) SaveRow)
                    |> List.concat
           )


decimals : Int -> List Int
decimals num =
    let
        dec n acc =
            if n > -10 && n < 10 then
                n :: acc

            else
                dec (n // 10) (remainderBy 10 n :: acc)
    in
    dec num []


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


equalLenList : Bool -> a -> List a -> List a -> ( List a, List a )
equalLenList prepend filler lst1 lst2 =
    let
        lst1len =
            List.length lst1

        lst2len =
            List.length lst2

        merge =
            if prepend then
                \a b -> a ++ b

            else
                \a b -> b ++ a
    in
    if lst1len > lst2len then
        ( lst1, merge (List.repeat (lst1len - lst2len) filler) lst2 )

    else
        ( merge (List.repeat (lst2len - lst1len) filler) lst1, lst2 )


nonzero : List number -> List number
nonzero =
    List.filter ((/=) 0)


maxColLen : List (List a) -> Int
maxColLen lst =
    Maybe.withDefault 1 <| List.maximum <| List.map List.length lst


errors : Multiplication -> Maybe AnnotatedMultiplication
errors m =
    let
        correct =
            correctResult m.multiplicand m.multiplier

        diff =
            { emptyAnnotatedMultiplication
                | multiplicand = m.multiplicand
                , multiplier = m.multiplier
                , resultRows =
                    List.map (\( a, b ) -> diffList True a b) <| zip givenResRows wantedResRows
                , upperRows = transpose <| fixedRows False 1 (IsOk 0) upperCols
                , sumUpperRow =
                    diffList True m.sumUpperRow correct.sumUpperRow
                , finalResult =
                    diffList True m.finalResult correct.finalResult
            }

        upperCols =
            List.map
                (\( a, b ) ->
                    let
                        ( fixedA, fixedB ) =
                            equalLenList False 0 (nonzero a) (nonzero b)
                    in
                    diffList False fixedA fixedB
                )
            <|
                zip
                    (transpose <| fixedRows True (maxColLen wantedUpRows) 0 givenUpRows)
                    (transpose <| fixedRows True (maxColLen givenUpRows) 0 wantedUpRows)

        ( givenResRows, wantedResRows ) =
            equalLenList False [] m.resultRows correct.resultRows

        ( givenUpRows, wantedUpRows ) =
            equalLenList False [] m.upperRows correct.upperRows

        ok =
            List.all identity
                [ List.all (List.all isOk) diff.resultRows
                , List.all (List.all isOk) diff.upperRows
                , List.all isOk diff.sumUpperRow
                , List.all isOk diff.finalResult
                ]
    in
    if ok then
        Nothing

    else
        Just diff


fixedRows : Bool -> Int -> a -> List (List a) -> List (List a)
fixedRows prepend min filler lst =
    let
        upto =
            max min <|
                Maybe.withDefault min <|
                    List.maximum <|
                        List.map List.length lst
    in
    if prepend then
        List.map (\l -> List.repeat (upto - List.length l) filler ++ l) lst

    else
        List.map (\l -> l ++ List.repeat (upto - List.length l) filler) lst


isOk : CheckedDigit -> Bool
isOk d =
    case d of
        IsOk _ ->
            True

        _ ->
            False


diffList : Bool -> List Int -> List Int -> List CheckedDigit
diffList prepend given wanted =
    equalLenList prepend 0 given wanted
        |> (\( l2, r2 ) ->
                List.map
                    (\( le, re ) ->
                        if le == re then
                            IsOk le

                        else
                            IsWrong le re
                    )
                    (zip l2 r2)
           )


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
    let
        op =
            m.currentOperation

        multiplicandDigits =
            decimals m.currentOperation.multiplicand

        multiplierDigits =
            decimals m.currentOperation.multiplier
    in
    case msg of
        NewMultiplication ( n, n2 ) ->
            let
                rowLen =
                    List.length (decimals n) + List.length (decimals n2)

                emptyRow =
                    List.repeat rowLen Nothing

                newFocus =
                    FocusedResultRow (rowLen - 1) 0
            in
            ( { m
                | currentOperation =
                    { op
                        | multiplicand = n
                        , multiplier = n2
                        , sumUpperRow = emptyRow
                        , finalResult = emptyRow
                    }
                , focused = newFocus
              }
            , Task.attempt (\_ -> NoOp) (Dom.focus <| colId <| focused2ColId newFocus)
            )

        UpperRowInput row col mval ->
            let
                newUpperRows =
                    case getAt row op.upperRows of
                        Just r ->
                            setAt row (setAt col mval r) op.upperRows

                        Nothing ->
                            List.append op.upperRows [ setAt col mval <| List.repeat (List.length multiplicandDigits + 1) Nothing ]
            in
            ( { m | currentOperation = { op | upperRows = newUpperRows } }, Cmd.none )

        ResultRowInput row col mval ->
            let
                newResultRows =
                    case getAt row op.resultRows of
                        Just r ->
                            setAt row (setAt col mval r) op.resultRows

                        Nothing ->
                            List.append
                                op.resultRows
                                [ setAt col mval <|
                                    List.repeat
                                        (List.length multiplicandDigits + List.length multiplierDigits)
                                        Nothing
                                ]
            in
            ( { m | currentOperation = { op | resultRows = newResultRows } }, Cmd.none )

        SumUpperRowInput col mval ->
            let
                newSumUpperRow =
                    setAt col mval op.sumUpperRow
            in
            ( { m | currentOperation = { op | sumUpperRow = newSumUpperRow } }, Cmd.none )

        FinalResultRowInput col mval ->
            let
                newFinalResult =
                    setAt col mval op.finalResult
            in
            ( { m | currentOperation = { op | finalResult = newFinalResult } }, Cmd.none )

        CheckResult ->
            let
                nonzeroUpperRows =
                    transpose <|
                        fixedRows True 0 Nothing <|
                            List.map (List.filter (\mv -> mv /= Just 0 && mv /= Nothing)) <|
                                transpose op.upperRows

                newOp =
                    { op | upperRows = nonzeroUpperRows }
            in
            ( { m
                | currentOperation = newOp
                , checked = True
                , errors =
                    Animator.interrupt
                        [ Animator.event Animator.immediately (errors (input2multiplication newOp))
                        , Animator.wait (Animator.millis 950)
                        , Animator.event Animator.immediately Nothing
                        ]
                        m.errors
              }
            , Cmd.none
            )

        AnimationTick newTime ->
            ( Animator.update newTime animator m, Cmd.none )

        Focus dir ->
            let
                newFocused =
                    changeFocus op m.focused dir
            in
            ( { m | focused = newFocused }
            , Task.attempt (\_ -> NoOp) (Dom.focus <| colId <| focused2ColId newFocused)
            )

        ForceFocus cid ->
            ( { m | focused = colId2focused cid }, Cmd.none )

        NoOp ->
            ( m, Cmd.none )


changeFocus : MultiplicationInput -> Focused -> FocusDirection -> Focused
changeFocus op focused dir =
    let
        upperRowsLen =
            List.length op.upperRows + 1

        resultRowsLen =
            List.length op.resultRows + 1

        upperRowLen =
            1 + (List.length <| decimals op.multiplicand)

        rowLen =
            List.length (decimals op.multiplier) + List.length (decimals op.multiplicand)
    in
    case ( focused, dir ) of
        ( FocusedUpperRow x y, FocusUp ) ->
            if y < upperRowsLen - 1 then
                FocusedUpperRow x (y + 1)

            else
                FocusedUpperRow x (upperRowsLen - 1)

        ( FocusedUpperRow x y, FocusDown ) ->
            if y > 0 then
                FocusedUpperRow x (y - 1)

            else if resultRowsLen > 2 then
                FocusedSumUpperRow (x + 1)

            else
                FocusedResultRow (x + 1) 0

        ( FocusedUpperRow x y, FocusLeft ) ->
            if x > 0 then
                FocusedUpperRow (x - 1) y

            else
                FocusedUpperRow 0 y

        ( FocusedUpperRow x y, FocusRight ) ->
            if x < upperRowLen - 1 then
                FocusedUpperRow (x + 1) y

            else
                FocusedUpperRow (upperRowLen - 1) y

        ( FocusedResultRow x y, FocusUp ) ->
            if y > 0 then
                FocusedResultRow x (y - 1)

            else if resultRowsLen > 2 then
                FocusedSumUpperRow x

            else
                FocusedUpperRow (max 0 (min (x - 1) (upperRowLen - 1))) 0

        ( FocusedResultRow x y, FocusDown ) ->
            if y < resultRowsLen - 1 then
                FocusedResultRow x (y + 1)

            else if resultRowsLen > 2 then
                FocusedFinalRow x

            else
                FocusedResultRow x (resultRowsLen - 1)

        ( FocusedResultRow x y, FocusLeft ) ->
            if x > 0 then
                FocusedResultRow (x - 1) y

            else
                FocusedResultRow 0 y

        ( FocusedResultRow x y, FocusRight ) ->
            if x < rowLen - 1 then
                FocusedResultRow (x + 1) y

            else
                FocusedResultRow (rowLen - 1) y

        ( FocusedSumUpperRow x, FocusUp ) ->
            FocusedUpperRow (max 0 (min (x - 1) (upperRowLen - 1))) 0

        ( FocusedSumUpperRow x, FocusDown ) ->
            FocusedResultRow x 0

        ( FocusedSumUpperRow x, FocusLeft ) ->
            if x > 0 then
                FocusedSumUpperRow (x - 1)

            else
                FocusedSumUpperRow 0

        ( FocusedSumUpperRow x, FocusRight ) ->
            if x < rowLen - 1 then
                FocusedSumUpperRow (x + 1)

            else
                FocusedSumUpperRow (rowLen - 1)

        ( FocusedFinalRow x, FocusUp ) ->
            if resultRowsLen > 2 then
                FocusedResultRow x (resultRowsLen - 1)

            else
                FocusedUpperRow (max 0 (min (x - 1) (upperRowLen - 1))) 0

        ( FocusedFinalRow x, FocusDown ) ->
            FocusedFinalRow x

        ( FocusedFinalRow x, FocusLeft ) ->
            if x > 0 then
                FocusedFinalRow (x - 1)

            else
                FocusedFinalRow 0

        ( FocusedFinalRow x, FocusRight ) ->
            if x < rowLen - 1 then
                FocusedFinalRow (x + 1)

            else
                FocusedFinalRow (rowLen - 1)

        ( FocusedNothing, _ ) ->
            FocusedResultRow 0 (rowLen - 1)


animator : Animator.Animator Model
animator =
    Animator.animator
        |> Animator.watchingWith
            .errors
            (\newErrors m -> { m | errors = newErrors })
            (\merrors ->
                case merrors of
                    Nothing ->
                        False

                    _ ->
                        True
            )


input2multiplication : MultiplicationInput -> Multiplication
input2multiplication input =
    let
        resultLength =
            List.length input.resultRows

        fixMaybes =
            List.map (Maybe.withDefault 0)
    in
    { multiplicand = input.multiplicand
    , multiplier = input.multiplier
    , resultRows = List.map fixMaybes input.resultRows
    , upperRows = List.map fixMaybes input.upperRows
    , sumUpperRow = fixMaybes input.sumUpperRow
    , finalResult =
        fixMaybes <|
            if resultLength == 1 then
                Maybe.withDefault [] <| List.head input.resultRows

            else
                input.finalResult
    }


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
            [ calculationView m
            , operationView m
            ]


upperRowStyle : List (Element.Attribute Msg)
upperRowStyle =
    [ Font.size 12
    , Font.color (Element.rgb255 200 10 10)
    , Element.htmlAttribute (Html.style "direction" "rtl")
    , Element.htmlAttribute (Html.style "text-align" "center")
    , Element.htmlAttribute (Html.style "height" "20px")
    , Element.alignRight
    ]


resultRowStyle : List (Element.Attribute Msg)
resultRowStyle =
    [ Font.size 20
    , Font.color (Element.rgb255 10 10 200)
    , Element.htmlAttribute (Html.style "direction" "rtl")
    , Element.htmlAttribute (Html.style "text-align" "center")
    , Element.alignRight
    ]


zipWithDefault : a -> b -> List a -> List b -> List ( a, b )
zipWithDefault defa defb a b =
    let
        aLen =
            List.length a

        bLen =
            List.length b
    in
    if bLen > aLen then
        zip (a ++ List.repeat (bLen - aLen) defa) b

    else
        zip a (b ++ List.repeat (aLen - bLen) defb)


calculationView : Model -> Element.Element Msg
calculationView m =
    let
        multiplicandDigits =
            decimals m.currentOperation.multiplicand

        multiplierDigits =
            decimals m.currentOperation.multiplier

        digitsColsNum =
            List.length multiplicandDigits + 1

        resultColsNum =
            List.length multiplierDigits + List.length multiplicandDigits

        animatedInputRow =
            renderInputRow m.errors

        resultRowsLen =
            max
                (List.length m.currentOperation.resultRows)
                (List.length diff.resultRows)

        upperRowsLen =
            max
                (List.length m.currentOperation.upperRows)
                (List.length diff.upperRows)

        lastLine mop =
            animatedInputRow
                { rid = ResultRow resultRowsLen
                , numEl = resultColsNum
                , style = resultRowStyle
                , operator = mop
                , action = ResultRowInput resultRowsLen
                }
                []
                Nothing

        moreThan1ResultRow =
            List.length m.currentOperation.resultRows
                > 1
                || List.length diff.resultRows
                > 1

        diff =
            case Animator.current m.errors of
                Nothing ->
                    { resultRows = List.repeat (List.length m.currentOperation.resultRows) Nothing
                    , upperRows = List.repeat (List.length m.currentOperation.upperRows) Nothing
                    , sumUpperRow = Nothing
                    , finalResult = Nothing
                    }

                Just d ->
                    { resultRows = List.map Just d.resultRows
                    , upperRows = List.map Just d.upperRows
                    , sumUpperRow = Just d.sumUpperRow
                    , finalResult = Just d.finalResult
                    }
    in
    Element.el
        [ Element.width Element.fill
        , Element.height Element.fill
        ]
    <|
        Keyed.column []
            (( rowId (UpperRow upperRowsLen)
             , animatedInputRow
                { rid = UpperRow upperRowsLen
                , numEl = digitsColsNum
                , style = upperRowStyle
                , operator = Nothing
                , action = UpperRowInput upperRowsLen
                }
                []
                Nothing
             )
                :: List.reverse
                    (List.indexedMap
                        (\i ( r, d ) ->
                            ( rowId (UpperRow i)
                            , animatedInputRow
                                { rid = UpperRow i
                                , numEl = digitsColsNum
                                , style = upperRowStyle
                                , operator = Nothing
                                , action = UpperRowInput i
                                }
                                r
                                d
                            )
                        )
                     <|
                        zipWithDefault [] Nothing m.currentOperation.upperRows diff.upperRows
                    )
                ++ [ ( "multiplicand", textNumber multiplicandDigits )
                   , ( "multiplier", operationLine "×" [ textNumber multiplierDigits ] )
                   ]
                ++ (if moreThan1ResultRow then
                        [ ( rowId SumUpperRow
                          , animatedInputRow
                                { rid = SumUpperRow
                                , numEl = resultColsNum
                                , style = upperRowStyle
                                , operator = Nothing
                                , action = SumUpperRowInput
                                }
                                m.currentOperation.sumUpperRow
                                diff.sumUpperRow
                          )
                        ]

                    else
                        []
                   )
                ++ (List.indexedMap
                        (\i ( r, d ) ->
                            ( rowId (ResultRow i)
                            , animatedInputRow
                                { rid = ResultRow i
                                , numEl = resultColsNum
                                , style = resultRowStyle
                                , operator = Nothing
                                , action = ResultRowInput i
                                }
                                r
                                d
                            )
                        )
                    <|
                        zipWithDefault [] Nothing m.currentOperation.resultRows diff.resultRows
                   )
                ++ (if moreThan1ResultRow then
                        [ ( rowId (ResultRow resultRowsLen), lastLine (Just "+") )
                        , ( rowId FinalRow
                          , animatedInputRow
                                { rid = FinalRow
                                , numEl = resultColsNum
                                , style = resultRowStyle
                                , operator = Nothing
                                , action = FinalResultRowInput
                                }
                                m.currentOperation.finalResult
                                diff.finalResult
                          )
                        ]

                    else
                        [ ( rowId (ResultRow resultRowsLen), lastLine Nothing ) ]
                   )
            )


operatorRowStyle : List (Element.Attribute Msg)
operatorRowStyle =
    [ Border.widthEach { bottom = 1, left = 0, right = 0, top = 0 }
    , Border.solid
    ]


operatorEl : String -> Element.Element Msg
operatorEl op =
    Element.el [ Element.width (Element.px 20) ] <| Element.text op


operationLine : String -> List (Element.Element Msg) -> Element.Element Msg
operationLine operator children =
    Element.row
        (operatorRowStyle
            ++ [ Element.spacing 0
               , Element.width Element.fill
               ]
        )
        (operatorEl operator :: children)


type alias RenderInputRowOptions =
    { rid : RowId
    , style : List (Element.Attribute Msg)
    , numEl : Int
    , action : Int -> Maybe Int -> Msg
    , operator : Maybe String
    }


renderInputRow :
    Animator.Timeline state
    -> RenderInputRowOptions
    -> List (Maybe Int)
    -> Maybe (List CheckedDigit)
    -> Element.Element Msg
renderInputRow timeline opts elements mdiff =
    Keyed.row
        ([ Element.width Element.fill
         , Font.variant Font.tabularNumbers
         , Element.spacing 0
         , Element.padding 0
         ]
            ++ (case opts.operator of
                    Just op ->
                        operatorRowStyle

                    Nothing ->
                        []
               )
        )
    <|
        (case opts.operator of
            Just op ->
                [ ( "operator" ++ op, operatorEl op ) ]

            Nothing ->
                []
        )
            ++ (List.indexedMap
                    (\idx ( mn, d ) ->
                        let
                            cid =
                                ColId opts.rid idx

                            elId =
                                colId cid
                        in
                        ( elId
                        , Input.text
                            ([ Element.width (Element.px 20)
                             , Font.color (Element.rgb255 200 10 10)
                             , Element.height (Element.px 20)
                             , Element.padding 0
                             , Element.pointer
                             , Element.focused [ Background.color (Element.rgba 1 1 1 0.5) ]
                             , Element.mouseOver [ Background.color (Element.rgba 1 1 1 0.25) ]
                             , Events.onFocus <| ForceFocus cid
                             , Element.htmlAttribute <| Html.id elId
                             , Element.htmlAttribute
                                (Keyboard.on Keyboard.Keydown
                                    [ ( ArrowUp, Focus FocusUp )
                                    , ( ArrowDown, Focus FocusDown )
                                    , ( ArrowLeft, Focus FocusLeft )
                                    , ( ArrowRight, Focus FocusRight )
                                    , ( Enter, CheckResult )
                                    ]
                                )
                             , Background.color
                                (if isOk d then
                                    Element.rgba 1 1 1 0

                                 else
                                    Element.rgba 1 0 0 <|
                                        Animator.linear timeline <|
                                            \_ ->
                                                Animator.wave 0 0.65 |> Animator.loop (Animator.millis 900)
                                )
                             , Border.width 0
                             ]
                                ++ opts.style
                            )
                            { onChange = opts.action idx << Maybe.map (remainderBy 10) << String.toInt
                            , text = Maybe.withDefault "" <| Maybe.map String.fromInt mn
                            , placeholder = Nothing
                            , label = Input.labelHidden ""
                            }
                        )
                    )
                <|
                    zip
                        (List.repeat (opts.numEl - List.length elements) Nothing
                            ++ elements
                        )
                        (case mdiff of
                            Nothing ->
                                List.repeat opts.numEl (IsOk 0)

                            Just diff ->
                                List.repeat (opts.numEl - List.length diff) (IsOk 0) ++ diff
                        )
               )


textNumber : List Int -> Element.Element Msg
textNumber digits =
    Element.row [ Element.width Element.fill, Font.variant Font.tabularNumbers ] <|
        List.map
            (\d ->
                Element.el [ Font.center, Element.alignRight, Element.width (Element.px 20) ] <|
                    Element.text <|
                        String.fromInt d
            )
            digits


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
        { onPress = Just CheckResult
        , label = Element.text "Sprawdź"
        }
