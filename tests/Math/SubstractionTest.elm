module Math.SubstractionTest exposing (calculatingResultSuite)

import Array
import Expect exposing (Expectation)
import Fuzz
import Math.Substraction as Substraction
import Test exposing (..)


calculatingResultSuite : Test
calculatingResultSuite =
    describe "checking detailed and correct substraction results"
        [ test "12 - 12" <|
            \_ ->
                let
                    result =
                        { minuend = 12
                        , subtrahend = 12
                        , upperRow1 = Array.repeat 2 Nothing
                        , upperRow2 = Array.repeat 2 Nothing
                        , result = 0
                        }
                in
                Substraction.correctResult 12 12 |> Expect.equal result
        , test "1000 - 1" <|
            \_ ->
                let
                    result =
                        { minuend = 1000
                        , subtrahend = 1
                        , upperRow1 = Array.fromList [ Just 0, Just 9, Just 9, Just 10 ]
                        , upperRow2 = Array.repeat 4 Nothing
                        , result = 999
                        }
                in
                Substraction.correctResult 1000 1 |> Expect.equal result
        , test "7652 - 661" <|
            \_ ->
                let
                    result =
                        { minuend = 7652
                        , subtrahend = 661
                        , upperRow1 = Array.fromList [ Just 6, Just 5, Just 15, Nothing ]
                        , upperRow2 = Array.fromList [ Nothing, Just 15, Nothing, Nothing ]
                        , result = 6991
                        }
                in
                Substraction.correctResult 7652 661 |> Expect.equal result
        , test "12 - 0" <|
            \_ ->
                let
                    result =
                        { minuend = 12
                        , subtrahend = 0
                        , upperRow1 = Array.repeat 2 Nothing
                        , upperRow2 = Array.repeat 2 Nothing
                        , result = 12
                        }
                in
                Substraction.correctResult 12 0 |> Expect.equal result
        ]
