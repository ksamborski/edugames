module MultiplicationTest exposing (calculatingResultSuite, decimalsSuite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Multiplication
import Test exposing (..)


decimalsSuite : Test
decimalsSuite =
    describe "checking splitting number into list of decimals"
        [ test "1" <|
            \_ -> Multiplication.decimals 1 |> Expect.equal [ 1 ]
        , test "12" <|
            \_ -> Multiplication.decimals 12 |> Expect.equal [ 1, 2 ]
        , test "512" <|
            \_ -> Multiplication.decimals 512 |> Expect.equal [ 5, 1, 2 ]
        ]


calculatingResultSuite : Test
calculatingResultSuite =
    describe "checking correct multiplication results"
        [ test "83 * 97" <|
            \_ ->
                let
                    result =
                        { multiplicand = 83
                        , multiplier = 97
                        , resultRows = [ [ 7, 4, 7 ], [ 5, 8, 1 ] ]
                        , upperRows = [ [ 7, 2 ], [ 5, 2 ] ]
                        , sumUpperRow = [ 1, 1, 0 ]
                        , finalResult = [ 1, 3, 2, 8 ]
                        }
                in
                Multiplication.correctResult 83 97 |> Expect.equal result
        , test "12 * 9" <|
            \_ ->
                let
                    result =
                        { multiplicand = 12
                        , multiplier = 9
                        , resultRows = [ [ 1, 0, 8 ] ]
                        , upperRows = [ [ 1, 1 ] ]
                        , sumUpperRow = [ 0, 0, 0 ]
                        , finalResult = [ 1, 0, 8 ]
                        }
                in
                Multiplication.correctResult 12 9 |> Expect.equal result
        ]
