port module Headless exposing (main)

import Array exposing (Array)
import Day exposing (Day(..))
import Day1
import Day2
import Dict exposing (Dict)
import Inputs
import Solution exposing (Solution)


port sendSolutionOutput : String -> Cmd msg


type alias Flags =
    List String


type alias DaySolution =
    ( Day, ( Solution, Solution ) )


type alias Model =
    ()


inputs : Array String
inputs =
    Array.fromList Inputs.days


inputForDay : Day -> Maybe String
inputForDay day =
    Array.get (Day.toInt day - 1) inputs


solvers : Dict Int ( () -> Solution, () -> Solution )
solvers =
    [ ( Day1
      , Day1.solvePartOne >> Solution.fromInt
      , Day1.solvePartTwo >> Solution.fromInt
      )
    , ( Day2
      , Day2.solvePartOne >> Solution.fromIntResult
      , Day2.solvePartTwo >> Solution.fromIntResult
      )
    ]
        |> List.map
            (\( day, a, b ) ->
                Maybe.map
                    (\input ->
                        ( Day.toInt day
                        , ( always input >> a
                          , always input >> b
                          )
                        )
                    )
                    (inputForDay day)
            )
        |> List.filterMap identity
        |> Dict.fromList


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
        |> Maybe.andThen Day.fromInt
        |> Result.fromMaybe ("Unknown day " ++ str)


daySolutionToString : DaySolution -> String
daySolutionToString ( day, ( partOne, partTwo ) ) =
    Day.toString day
        ++ ""
        ++ "\n├─ Part one "
        ++ Solution.toString partOne
        ++ "\n├─ Part two "
        ++ Solution.toString partTwo


solveDay : Day -> DaySolution
solveDay day =
    case Dict.get (Day.toInt day) solvers of
        Just ( solvePartOne, solvePartTwo ) ->
            ( day
            , ( solvePartOne ()
              , solvePartTwo ()
              )
            )

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
