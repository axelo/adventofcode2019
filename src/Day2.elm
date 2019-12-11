module Day2 exposing (solve, solvePartOne, solvePartTwo)

import Array exposing (Array)
import Parser exposing (Parser)
import Solution exposing (Solution)


type alias Program =
    Array Int


type alias PC =
    Int


type Opcode
    = OpcodeAdd
    | OpcodeMul
    | OpcodeHalt


type Instruction
    = Add Int Int Int
    | Mul Int Int Int
    | Halt


type ProgramState
    = Running
    | Halted


solve : String -> ( Solution, Solution )
solve input =
    ( solvePartOne (Just 1202) input
    , solvePartTwo input
    )
        |> Tuple.mapBoth
            Solution.fromIntResult
            Solution.fromIntResult


solvePartOne : Maybe Int -> String -> Result String Int
solvePartOne maybeNounAndVerb input =
    input
        |> parseInput
        |> Result.andThen (runProgram maybeNounAndVerb)
        |> Result.andThen (Array.get 0 >> Result.fromMaybe "Missing index 0")


solvePartTwo : String -> Result String Int
solvePartTwo input =
    input
        |> parseInput
        |> Result.andThen (runUntilOutput 19690720 0)


setProgramNounAndVerb : Int -> Program -> Program
setProgramNounAndVerb nounAndVerb program =
    let
        ( noun, verb ) =
            splitNounAndVerb nounAndVerb
    in
    program
        |> Array.set 1 noun
        |> Array.set 2 verb


splitNounAndVerb : Int -> ( Int, Int )
splitNounAndVerb nounAndVerb =
    ( nounAndVerb // 100, modBy 100 nounAndVerb )


runUntilOutput : Int -> Int -> Program -> Result String Int
runUntilOutput wantedOutput nounAndVerb program =
    case runUntilOutputHelp nounAndVerb program of
        Ok val ->
            if val == wantedOutput then
                Ok nounAndVerb

            else
                runUntilOutput wantedOutput (nounAndVerb + 1) program

        Err err ->
            Err err


runUntilOutputHelp : Int -> Program -> Result String Int
runUntilOutputHelp nounAndVerb program =
    if nounAndVerb >= 100 * 100 then
        Err "Not Found"

    else
        program
            |> runProgram (Just nounAndVerb)
            |> Result.andThen (Array.get 0 >> Result.fromMaybe "Missing index 0")


runProgram : Maybe Int -> Program -> Result String Program
runProgram maybeNoundAndVerb program =
    runProgramHelp
        ( ( 0, Running )
        , case maybeNoundAndVerb of
            Just noundAndVerb ->
                setProgramNounAndVerb noundAndVerb program

            Nothing ->
                program
        )


runProgramHelp : ( ( PC, ProgramState ), Program ) -> Result String Program
runProgramHelp ( ( pc, programState ), program ) =
    case programState of
        Running ->
            fetchOpcode pc program
                |> Result.andThen (fetchInstruction program)
                |> Result.andThen (executeInstruction program)
                |> Result.andThen runProgramHelp

        Halted ->
            Ok program


fetchOpcode : PC -> Program -> Result String ( PC, Opcode )
fetchOpcode pc program =
    Array.get pc program
        |> Result.fromMaybe "Program Counter Out of Bounds"
        |> Result.andThen
            (\opcode ->
                case opcode of
                    1 ->
                        Ok ( pc + 1, OpcodeAdd )

                    2 ->
                        Ok ( pc + 1, OpcodeMul )

                    99 ->
                        Ok ( pc + 1, OpcodeHalt )

                    _ ->
                        Err ("Unknown opcode " ++ String.fromInt opcode)
            )


fetchInstruction : Program -> ( PC, Opcode ) -> Result String ( PC, Instruction )
fetchInstruction program ( pc, opcode ) =
    case opcode of
        OpcodeAdd ->
            readInstructionSize3 Add pc program
                |> Result.map (Tuple.pair (pc + 3))

        OpcodeMul ->
            readInstructionSize3 Mul pc program
                |> Result.map (Tuple.pair (pc + 3))

        OpcodeHalt ->
            Ok ( pc, Halt )


executeInstruction : Program -> ( PC, Instruction ) -> Result String ( ( PC, ProgramState ), Program )
executeInstruction program ( pc, instruction ) =
    case instruction of
        Add r1 r2 w1 ->
            executeInstructionSize3 r1 r2 w1 (+) program
                |> Result.map (Tuple.pair ( pc, Running ))

        Mul r1 r2 w1 ->
            executeInstructionSize3 r1 r2 w1 (*) program
                |> Result.map (Tuple.pair ( pc, Running ))

        Halt ->
            Ok ( ( pc, Halted ), program )


executeInstructionSize3 : Int -> Int -> Int -> (Int -> Int -> Int) -> Program -> Result String Program
executeInstructionSize3 readIndexA readIndexB writeIndex operation program =
    Maybe.map2
        (\val1 val2 ->
            program
                |> safeArraySet writeIndex (operation val1 val2)
                |> Result.fromMaybe "Write Index Out of Bounds"
        )
        (Array.get readIndexA program)
        (Array.get readIndexB program)
        |> Maybe.withDefault (Err "Read Index Out of Bounds")


readInstructionSize3 : (Int -> Int -> Int -> Instruction) -> PC -> Program -> Result String Instruction
readInstructionSize3 toInstruction pc program =
    Maybe.map3 toInstruction
        (Array.get pc program)
        (Array.get (pc + 1) program)
        (Array.get (pc + 2) program)
        |> Result.fromMaybe "Program Counter Out of Bounds"


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
