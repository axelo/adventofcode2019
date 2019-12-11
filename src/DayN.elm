module DayN exposing (solve, solvePartOne, solvePartTwo)

import Solution exposing (Solution)


type alias Distance =
    Int


solve : String -> ( Solution, Solution )
solve input =
    ( solvePartOne input
    , solvePartTwo input
    )
        |> Tuple.mapBoth
            Solution.fromIntResult
            Solution.fromIntResult


solvePartOne : String -> Result String Distance
solvePartOne _ =
    Err "Not implemented"


solvePartTwo : String -> Result String Distance
solvePartTwo _ =
    Err "Not implemented"
