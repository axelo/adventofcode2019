module Day1 exposing (solve, solvePartOne, solvePartTwo)

import Solution exposing (Solution)


type alias Mass =
    Int


type alias Fuel =
    Int


solve : String -> ( Solution, Solution )
solve input =
    ( solvePartOne input |> Solution.fromInt
    , solvePartTwo input |> Solution.fromInt
    )


solvePartOne : String -> Fuel
solvePartOne input =
    parseMass input
        |> List.map fuelFromMass
        |> List.sum


solvePartTwo : String -> Fuel
solvePartTwo input =
    parseMass input
        |> List.map (fuelFromMass >> fuelFromFuel)
        |> List.sum


parseMass : String -> List Mass
parseMass input =
    String.split "\n" input
        |> List.filterMap String.toInt


fuelFromMass : Mass -> Fuel
fuelFromMass mass =
    floor (toFloat mass / 3.0) - 2


fuelFromFuel : Fuel -> Fuel
fuelFromFuel fuel =
    if fuel <= 0 then
        0

    else
        fuel + fuelFromFuel (fuelFromMass fuel)
