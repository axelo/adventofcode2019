module Solution exposing
    ( Solution(..)
    , SolutionValue
    , fromInt
    , fromIntResult
    , solutionValueToString
    )


type Solution
    = NotTried
    | Solved SolutionValue
    | Failed String


type SolutionValue
    = StringVal String
    | IntVal Int


fromInt : Int -> Solution
fromInt =
    Solved << IntVal


fromIntResult : Result String Int -> Solution
fromIntResult result =
    case result of
        Ok val ->
            fromInt val

        Err err ->
            Failed err


solutionValueToString : SolutionValue -> String
solutionValueToString val =
    case val of
        StringVal s ->
            s

        IntVal i ->
            String.fromInt i
