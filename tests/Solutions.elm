module Solutions exposing (suite)

import Array
import Day1
import Day2
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
        , describe "Day2 - Part One"
            [ test "1,0,0,0,99 becomes 2,0,0,0,99" <|
                \_ ->
                    Day2.runProgram 0 (Array.fromList [ 1, 0, 0, 0, 99 ])
                        |> Expect.equal (Ok (Array.fromList [ 2, 0, 0, 0, 99 ]))
            , test "99,1,0,0,1,1,1,1 becomes 99,1,0,0,1,1,1,1" <|
                \_ ->
                    Day2.runProgram 100 (Array.fromList [ 99, 1, 0, 0, 1, 1, 1, 1 ])
                        |> Expect.equal (Ok (Array.fromList [ 99, 1, 0, 0, 1, 1, 1, 1 ]))
            , test "2,4,4,5,99,0 becomes 2,4,4,5,99,9801" <|
                \_ ->
                    Day2.runProgram 404 (Array.fromList [ 2, 4, 4, 5, 99, 0 ])
                        |> Expect.equal (Ok (Array.fromList [ 2, 4, 4, 5, 99, 9801 ]))
            , test "1,1,1,4,99,5,6,0,99 becomes 2,4,4,5,99,9801" <|
                \_ ->
                    Day2.runProgram 101 (Array.fromList [ 1, 1, 1, 4, 99, 5, 6, 0, 99 ])
                        |> Expect.equal (Ok (Array.fromList [ 30, 1, 1, 4, 2, 5, 6, 0, 99 ]))
            , test "1,12,2,4,1,2,3,4,1,2,3,4,4,99 becomes 2,4,4,5,99,9801" <|
                \_ ->
                    [ 1, 12, 2, 0, 1, 0, 0, 0, 1, 0, 0, 0, 99 ]
                        |> (Array.fromList >> Day2.runProgram 1202)
                        |> Expect.equal
                            ([ 404, 12, 2, 0, 1, 0, 0, 0, 1, 0, 0, 0, 99 ]
                                |> Array.fromList
                                |> Ok
                            )
            , test "runProgram1202" <|
                \_ ->
                    Day2.runProgram 1202 (Array.fromList [ 1, 99, 1337, 0, 1, 0, 0, 0, 1, 0, 0, 0, 99 ])
                        |> Expect.equal (Ok <| Array.fromList [ 404, 12, 2, 0, 1, 0, 0, 0, 1, 0, 0, 0, 99 ])
            , describe "Day2 - Part Two"
                [ test "Don't hang" <|
                    \_ ->
                        case Day2.runUntilOutput 19690720 0 (Array.fromList [ 1, 0, 0, 0, 1, 0, 0, 0, 99 ]) of
                            Ok _ ->
                                Expect.fail "Should have not found an input"

                            Err _ ->
                                Expect.pass
                ]
            ]
        ]
