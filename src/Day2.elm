module Day2 exposing (runProgram, runProgram1202, solvePartOne)

import Array exposing (Array)
import Parser exposing (Parser)


type alias Program =
    Array Int


type alias PC =
    Int


type Opcode
    = Add Int Int Int
    | Mul Int Int Int
    | Halt


solvePartOne : String -> Result String Int
solvePartOne input =
    input
        |> parseInput
        |> Result.andThen runProgram1202
        |> Result.andThen (Array.get 0 >> Result.fromMaybe "Missing index 0")


runProgram1202 : Program -> Result String Program
runProgram1202 program =
    program
        |> Array.set 1 12
        |> Array.set 2 2
        |> runProgram


runProgram : Program -> Result String Program
runProgram program =
    runProgramHelp 0 program


runProgramHelp : Int -> Program -> Result String Program
runProgramHelp pc program =
    case fetchOpcode program pc of
        Ok Halt ->
            Ok program

        Ok opcode ->
            executeOpcode program opcode
                |> Result.andThen (runProgramHelp (pc + 4))

        Err err ->
            Err err


fetchOpcode : Program -> PC -> Result String Opcode
fetchOpcode program pc =
    Array.get pc program
        |> Result.fromMaybe "Program Counter Out of Bounds"
        |> Result.andThen
            (\opcode ->
                case opcode of
                    1 ->
                        fetchSize3Opcode Add pc program

                    2 ->
                        fetchSize3Opcode Mul pc program

                    99 ->
                        Ok Halt

                    _ ->
                        Err ("Unknown opcode " ++ String.fromInt opcode)
            )


fetchSize3Opcode : (Int -> Int -> Int -> Opcode) -> PC -> Program -> Result String Opcode
fetchSize3Opcode toOpcode pc program =
    Maybe.map3 toOpcode
        (Array.get (pc + 1) program)
        (Array.get (pc + 2) program)
        (Array.get (pc + 3) program)
        |> Result.fromMaybe "Program Counter Out of Bounds"


executeOpcode : Program -> Opcode -> Result String Program
executeOpcode program opcode =
    case opcode of
        Add r1 r2 w1 ->
            executeOperation r1 r2 w1 (+) program

        Mul r1 r2 w1 ->
            executeOperation r1 r2 w1 (*) program

        Halt ->
            Ok program


executeOperation : Int -> Int -> Int -> (Int -> Int -> Int) -> Program -> Result String Program
executeOperation readIndexA readIndexB writeIndex operation program =
    Maybe.map2
        (\val1 val2 ->
            program
                |> safeArraySet writeIndex (operation val1 val2)
                |> Result.fromMaybe "Write Index Out of Bounds"
        )
        (Array.get readIndexA program)
        (Array.get readIndexB program)
        |> Maybe.withDefault (Err "Read Index Out of Bounds")


safeArraySet : Int -> a -> Array a -> Maybe (Array a)
safeArraySet index value program =
    Array.get index program
        |> Maybe.map
            (\_ ->
                Array.set index value program
            )


parseInput : String -> Result String Program
parseInput input =
    Parser.run parserInts input
        |> Result.map Array.fromList
        |> Result.mapError deadEndsToString


parserInts : Parser (List Int)
parserInts =
    Parser.loop [] parseIntsHelp


parseIntsHelp : List Int -> Parser (Parser.Step (List Int) (List Int))
parseIntsHelp revInts =
    Parser.int
        |> Parser.andThen
            (\i ->
                Parser.oneOf
                    [ Parser.end
                        |> Parser.map (always <| Parser.Done (i :: revInts |> List.reverse))
                    , Parser.symbol ","
                        |> Parser.map (always <| Parser.Loop (i :: revInts))
                    ]
            )


deadEndsToString : List Parser.DeadEnd -> String
deadEndsToString list =
    list
        |> List.map (\de -> "Problem at row " ++ String.fromInt de.row ++ " column " ++ String.fromInt de.col)
        |> String.join ". "
