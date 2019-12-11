port module Headless exposing (main)

import Day1
import Day2
import Day3
import DayN
import Dict exposing (Dict)
import Inputs
import Solution exposing (Solution)


port sendSolutionOutput : String -> Cmd msg


type alias Flags =
    List String


type alias Day =
    Int


type alias DaySolution =
    ( Day, ( Solution, Solution ) )


main : Program Flags () msg
main =
    Platform.worker
        { init = init
        , update = \_ model -> ( model, Cmd.none )
        , subscriptions = always Sub.none
        }


init : Flags -> ( (), Cmd msg )
init daysToSolve =
    ( ()
    , daysToSolve
        |> List.map dayFromString
        |> flattenResult []
        |> Result.map (List.map solveDay)
        |> (toOutput >> sendSolutionOutput)
    )


solvers : Dict Day (() -> ( Solution, Solution ))
solvers =
    [ ( 1, Day1.solve << always Inputs.day1 )
    , ( 2, Day2.solve << always Inputs.day2 )
    , ( 3, Day3.solve << always Inputs.day3 )
    , ( 4, DayN.solve << always Inputs.day4 )
    , ( 5, DayN.solve << always Inputs.day5 )
    , ( 6, DayN.solve << always Inputs.day6 )
    , ( 7, DayN.solve << always Inputs.day7 )
    ]
        |> Dict.fromList


toOutput : Result String (List DaySolution) -> String
toOutput result =
    case result of
        Ok daySolutions ->
            (List.map daySolutionToString daySolutions
                |> String.join "\n│\n"
            )
                ++ "\n│\n└ \u{1F976} !"

        Err reason ->
            reason


dayFromString : String -> Result String Day
dayFromString str =
    str
        |> String.toInt
        |> Maybe.andThen
            (\number ->
                if number >= 1 && number <= 25 then
                    Just number

                else
                    Nothing
            )
        |> Result.fromMaybe ("Unknown day " ++ str)


daySolutionToString : DaySolution -> String
daySolutionToString ( day, ( partOne, partTwo ) ) =
    String.fromInt day
        ++ ""
        ++ "\n├─ "
        ++ Solution.toString partOne
        ++ "\n├─ "
        ++ Solution.toString partTwo


solveDay : Day -> DaySolution
solveDay day =
    case Dict.get day solvers of
        Just solve ->
            ( day, solve () )

        Nothing ->
            ( day
            , ( Solution.Failed "Missing solver"
              , Solution.Failed "Missing solver"
              )
            )


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
