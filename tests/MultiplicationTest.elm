module MultiplicationTest exposing (calculatingResultSuite, decimalsSuite, errorsSuite, multiplicationSuite)

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
    describe "checking detailed and correct multiplication results"
        [ test "83 * 97" <|
            \_ ->
                let
                    result =
                        { multiplicand = 83
                        , multiplier = 97
                        , resultRows = [ [ 5, 8, 1 ], [ 7, 4, 7, 0 ] ]
                        , upperRows = [ [ 5, 2, 0 ], [ 7, 2, 0 ] ]
                        , sumUpperRow = [ 0, 1, 1, 0 ]
                        , finalResult = [ 8, 0, 5, 1 ]
                        }
                in
                Multiplication.correctResult 83 97 |> Expect.equal result
        , test "83 * 92" <|
            \_ ->
                let
                    result =
                        { multiplicand = 83
                        , multiplier = 92
                        , resultRows = [ [ 1, 6, 6 ], [ 7, 4, 7, 0 ] ]
                        , upperRows = [ [ 1, 0, 0 ], [ 7, 2, 0 ] ]
                        , sumUpperRow = [ 0, 1, 0, 0 ]
                        , finalResult = [ 7, 6, 3, 6 ]
                        }
                in
                Multiplication.correctResult 83 92 |> Expect.equal result
        , test "12 * 9" <|
            \_ ->
                let
                    result =
                        { multiplicand = 12
                        , multiplier = 9
                        , resultRows = [ [ 1, 0, 8 ] ]
                        , upperRows = [ [ 1, 1, 0 ] ]
                        , sumUpperRow = [ 0, 0, 0 ]
                        , finalResult = [ 1, 0, 8 ]
                        }
                in
                Multiplication.correctResult 12 9 |> Expect.equal result
        ]


errorsSuite : Test
errorsSuite =
    describe "checking verification algorithm"
        [ fuzz
            (Fuzz.map2 Tuple.pair
                (Fuzz.intRange 0 100000)
                (Fuzz.intRange 0 100000)
            )
            "checking correct result"
          <|
            \( n, m ) -> Multiplication.errors (Multiplication.correctResult n m) |> Expect.equal Nothing
        , test "83 * 92 (skipping 0s in the upper rows)" <|
            \_ ->
                let
                    result =
                        { multiplicand = 83
                        , multiplier = 92
                        , resultRows = [ [ 1, 6, 6 ], [ 7, 4, 7, 0 ] ]
                        , upperRows = [ [ 1, 2 ], [ 7, 0 ] ]
                        , sumUpperRow = [ 0, 1, 0, 0 ]
                        , finalResult = [ 7, 6, 3, 6 ]
                        }
                in
                Multiplication.errors result |> Expect.equal Nothing
        , fuzz
            (Fuzz.map2 Tuple.pair
                (Fuzz.intRange 1 100000)
                (Fuzz.intRange 1 100000)
            )
            "checking incorrect result"
          <|
            \( n, m ) ->
                let
                    correct =
                        Multiplication.correctResult n m
                in
                Multiplication.errors { correct | multiplicand = n + 1 } |> Expect.notEqual Nothing
        ]
