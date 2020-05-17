module Math.DivisionTest exposing (calculatingResultSuite)

import Array
import Expect exposing (Expectation)
import Fuzz
import Math.Division as Division
import Test exposing (..)


calculatingResultSuite : Test
calculatingResultSuite =
    describe "checking detailed and correct division results"
        [ test "12 / 12" <|
            \_ ->
                let
                    result =
                        { dividend = 12
                        , divisor = 12
                        , result = 1
                        , remainderRows =
                            [ { minuend = 12
                              , subtrahend = 12
                              , upperRow1 = Array.repeat 2 Nothing
                              , upperRow2 = Array.repeat 2 Nothing
                              , result = 0
                              }
                            ]
                        }
                in
                Division.correctResult 12 12 |> Expect.equal result
        , test "12 / 3" <|
            \_ ->
                let
                    result =
                        { dividend = 12
                        , divisor = 3
                        , result = 4
                        , remainderRows =
                            [ { minuend = 12
                              , subtrahend = 12
                              , upperRow1 = Array.repeat 2 Nothing
                              , upperRow2 = Array.repeat 2 Nothing
                              , result = 0
                              }
                            ]
                        }
                in
                Division.correctResult 12 3 |> Expect.equal result
        , test "12 / 5" <|
            \_ ->
                let
                    result =
                        { dividend = 12
                        , divisor = 5
                        , result = 2
                        , remainderRows =
                            [ { minuend = 12
                              , subtrahend = 10
                              , upperRow1 = Array.repeat 2 Nothing
                              , upperRow2 = Array.repeat 2 Nothing
                              , result = 2
                              }
                            ]
                        }
                in
                Division.correctResult 12 5 |> Expect.equal result
        , test "12 / 7" <|
            \_ ->
                let
                    result =
                        { dividend = 12
                        , divisor = 7
                        , result = 1
                        , remainderRows =
                            [ { minuend = 12
                              , subtrahend = 7
                              , upperRow1 = Array.fromList [ Just 0, Just 12 ]
                              , upperRow2 = Array.repeat 2 Nothing
                              , result = 5
                              }
                            ]
                        }
                in
                Division.correctResult 12 7 |> Expect.equal result
        , test "12 / 1" <|
            \_ ->
                let
                    result =
                        { dividend = 12
                        , divisor = 1
                        , result = 12
                        , remainderRows =
                            [ { minuend = 1
                              , subtrahend = 1
                              , upperRow1 = Array.repeat 1 Nothing
                              , upperRow2 = Array.repeat 1 Nothing
                              , result = 0
                              }
                            , { minuend = 2
                              , subtrahend = 2
                              , upperRow1 = Array.repeat 1 Nothing
                              , upperRow2 = Array.repeat 1 Nothing
                              , result = 0
                              }
                            ]
                        }
                in
                Division.correctResult 12 1 |> Expect.equal result
        ]
