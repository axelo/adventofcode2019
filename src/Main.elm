module Main exposing (main)

import Array exposing (Array)
import Browser
import Day1
import Day2
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Inputs
import Solution exposing (Solution)


type alias Model =
    Array Day


type alias Day =
    { input : String
    , partOne : Solution
    , partTwo : Solution
    }


type alias DayNumber =
    Int


type alias Solver =
    { partOne : String -> Solution
    , partTwo : String -> Solution
    , startInput : String
    }


type Msg
    = SolvePartOne DayNumber
    | SolvePartTwo DayNumber
    | SetInput DayNumber String


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }


solvers : Array Solver
solvers =
    Array.fromList
        [ Solver
            (Day1.solvePartOne >> Solution.fromInt)
            (Day1.solvePartTwo >> Solution.fromInt)
            Inputs.day1
        , Solver
            (Day2.solvePartOne >> Solution.fromIntResult)
            (Day2.solvePartTwo >> Solution.fromIntResult)
            Inputs.day2
        ]


notImplemented : String -> Solution
notImplemented =
    always (Solution.Failed "Not implemented yet")


initDay : String -> Day
initDay input =
    Day input Solution.NotTried Solution.NotTried


setDayInput : String -> Day -> Day
setDayInput input day =
    { day | input = input }


solvePartOne : DayNumber -> Day -> Day
solvePartOne dayNumber day =
    { day
        | partOne =
            (Array.get dayNumber solvers
                |> Maybe.map .partOne
                |> Maybe.withDefault notImplemented
            )
                day.input
    }


solvePartTwo : DayNumber -> Day -> Day
solvePartTwo dayNumber day =
    { day
        | partTwo =
            (Array.get dayNumber solvers
                |> Maybe.map .partTwo
                |> Maybe.withDefault notImplemented
            )
                day.input
    }


updateArray : Int -> (a -> a) -> Array a -> Array a
updateArray index set array =
    (Array.get index array
        |> Maybe.map (set >> Array.set index)
        |> Maybe.withDefault identity
    )
        array


init : () -> ( Model, Cmd msg )
init _ =
    ( Array.map (.startInput >> initDay) solvers
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SolvePartOne dayNumber ->
            ( updateArray dayNumber (solvePartOne dayNumber) model
            , Cmd.none
            )

        SolvePartTwo dayNumber ->
            ( updateArray dayNumber (solvePartTwo dayNumber) model
            , Cmd.none
            )

        SetInput dayNumber input ->
            ( updateArray dayNumber (setDayInput input) model
            , Cmd.none
            )


view : Model -> Browser.Document Msg
view model =
    { title = "Advent of Code 2019"
    , body = [ viewBody model ]
    }


viewBody : Model -> Html Msg
viewBody model =
    Html.div
        [ Attributes.class "w-full h-full bg-blue-900 text-gray-100 font-body text-base"
        ]
        [ Html.div
            [ Attributes.class "p-2 mx-auto max-w-lg"
            ]
            [ Html.header [] []
            , Html.main_ []
                (Array.toList
                    (Array.indexedMap viewDay model)
                )
            ]
        ]


viewDay : DayNumber -> Day -> Html Msg
viewDay dayNumber { input, partOne, partTwo } =
    Html.section
        [ Attributes.class ""
        ]
        [ Html.h1
            [ Attributes.class "text-white"
            ]
            [ Html.text ("--- Day  " ++ String.fromInt (dayNumber + 1) ++ " ---")
            ]
        , Html.article [ Attributes.class "py-2" ]
            [ Html.textarea
                [ Attributes.class "block bg-black border border-gray-200 w-full"
                , Attributes.rows 5
                , Attributes.value input
                , Events.onInput (SetInput dayNumber)
                ]
                []
            , Html.div [ Attributes.class "mt-2" ]
                [ Html.button
                    [ Attributes.class "text-green-200 hover:text-green-100"
                    , Events.onClick (SolvePartOne dayNumber)
                    ]
                    [ Html.text "[Solve Part One]" ]
                , viewSolution partOne
                ]
            , Html.button
                [ Attributes.class "text-green-200 hover:text-green-100"
                , Events.onClick (SolvePartTwo dayNumber)
                ]
                [ Html.text "[Solve Part Two]" ]
            , viewSolution partTwo
            ]
        ]


viewSolution : Solution -> Html msg
viewSolution solution =
    case solution of
        Solution.NotTried ->
            Html.span
                [ Attributes.class "ml-2"
                ]
                [ Html.text "" ]

        Solution.Solved value ->
            Html.span
                [ Attributes.class "ml-2 text-yellow text-shadow-yellow"
                ]
                [ Html.text (Solution.solutionValueToString value) ]

        Solution.Failed reason ->
            Html.span
                [ Attributes.class "ml-2 text-red"
                ]
                [ Html.text reason ]
