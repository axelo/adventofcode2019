module Day1 exposing (Fuel, fuelToString, solvePartOne, solvePartTwo)


type alias Mass =
    Int


type alias Fuel =
    Int


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


fuelToString : Fuel -> String
fuelToString =
    String.fromInt


fuelFromMass : Mass -> Fuel
fuelFromMass mass =
    floor (toFloat mass / 3.0) - 2


fuelFromFuel : Fuel -> Fuel
fuelFromFuel fuel =
    if fuel <= 0 then
        0

    else
        fuel + fuelFromFuel (fuelFromMass fuel)
