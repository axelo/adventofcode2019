module Solutions exposing (suite)

import Day1
import Expect
import Test exposing (..)


suite : Test
suite =
    describe "Days"
        [ describe "Day1 - Part One"
            [ test "12 requires 2" <|
                \_ -> Day1.solvePartOne "12" |> Expect.equal 2
            , test "14 requires 2" <|
                \_ -> Day1.solvePartOne "14" |> Expect.equal 2
            , test "1969 requires 654" <|
                \_ -> Day1.solvePartOne "1969" |> Expect.equal 654
            , test "100756 requires 33583" <|
                \_ -> Day1.solvePartOne "100756" |> Expect.equal 33583
            ]
        , describe "Day1 - Part Two"
            [ test "14 requires 2" <|
                \_ -> Day1.solvePartTwo "14" |> Expect.equal 2
            , test "1969 requires 966" <|
                \_ -> Day1.solvePartTwo "1969" |> Expect.equal 966
            , test "100756 requires 50346" <|
                \_ -> Day1.solvePartTwo "100756" |> Expect.equal 50346
            ]
        ]
