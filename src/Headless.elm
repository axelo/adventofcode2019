port module Headless exposing (main)

import Day1
import Day2
import Inputs
import Solution exposing (Solution)


port sendSolutionOutput : String -> Cmd msg


type Day
    = Day1
    | Day2


type alias Flags =
    List String


type alias DaySolution =
    ( Day, ( Solution, Solution ) )


type alias Model =
    ()


main : Program Flags Model msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = always Sub.none
        }


init : Flags -> ( Model, Cmd msg )
init daysToSolve =
    ( ()
    , daysToSolve
        |> List.map dayFromString
        |> flattenResult []
        |> Result.map (List.map solveDay)
        |> (toOutput >> sendSolutionOutput)
    )


update : msg -> Model -> ( Model, Cmd msg )
update _ model =
    ( model, Cmd.none )


toOutput : Result String (List DaySolution) -> String
toOutput result =
    case result of
        Ok daySolutions ->
            List.map daySolutionToString daySolutions
                |> String.join "\n\n"

        Err reason ->
            reason


daySolutionToString : DaySolution -> String
daySolutionToString ( day, ( partOne, partTwo ) ) =
    dayToString day
        ++ ""
        ++ "\n├─ Part one "
        ++ Solution.toString partOne
        ++ "\n└─ Part two "
        ++ Solution.toString partTwo


dayToString : Day -> String
dayToString day =
    case day of
        Day1 ->
            "Day 1"

        Day2 ->
            "Day 2"


solveDay : Day -> DaySolution
solveDay day =
    ( day
    , case day of
        Day1 ->
            ( Day1.solvePartOne Inputs.day1 |> Solution.fromInt
            , Day1.solvePartTwo Inputs.day1 |> Solution.fromInt
            )

        Day2 ->
            ( Day2.solvePartOne Inputs.day2 |> Solution.fromIntResult
            , Day2.solvePartTwo Inputs.day2 |> Solution.fromIntResult
            )
    )


dayFromString : String -> Result String Day
dayFromString day =
    case day of
        "1" ->
            Ok Day1

        "2" ->
            Ok Day2

        _ ->
            Err ("Couldn't understand day " ++ day)


flattenResult : List a -> List (Result String a) -> Result String (List a)
flattenResult days results =
    case results of
        [] ->
            Ok (List.reverse days)

        result :: rest ->
            case result of
                Ok day ->
                    flattenResult (day :: days) rest

                Err reason ->
                    Err reason
