module Day exposing (Day(..), fromInt, toInt, toString)


type Day
    = Day1
    | Day2
    | Day3


toInt : Day -> Int
toInt day =
    case day of
        Day1 ->
            1

        Day2 ->
            2

        Day3 ->
            3


fromInt : Int -> Maybe Day
fromInt number =
    case number of
        1 ->
            Just Day1

        2 ->
            Just Day2

        3 ->
            Just Day3

        _ ->
            Nothing


toString : Day -> String
toString day =
    case day of
        Day1 ->
            "Day 1"

        Day2 ->
            "Day 2"

        Day3 ->
            "Day 3"
