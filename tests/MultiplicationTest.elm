module MultiplicationTest exposing (calculatingResultSuite, decimalsSuite, multiplicationSuite)

import Expect exposing (Expectation)
import Fuzz
import Multiplication
import Test exposing (..)


multiplicationSuite : Test
multiplicationSuite =
    fuzz
        (Fuzz.map2 Tuple.pair (Fuzz.intRange 0 100000) (Fuzz.intRange 0 100000))
        "checking multiplication results"
    <|
        \( n, m ) ->
            Expect.equal
                (Multiplication.decimals (n * m))
                (Multiplication.correctResult n m).finalResult


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
                        , resultRows = [ [ 7, 4, 7, 0 ], [ 5, 8, 1 ] ]
                        , upperRows = [ [ 7, 2 ], [ 5, 2 ] ]
                        , sumUpperRow = [ 0, 1, 1, 0 ]
                        , finalResult = [ 8, 0, 5, 1 ]
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
