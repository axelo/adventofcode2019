module Solution exposing
    ( Solution(..)
    , SolutionValue
    , fromInt
    , fromIntResult
    , toString
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


toString : Solution -> String
toString solution =
    case solution of
        NotTried ->
            "Not tried"

        Solved value ->
            solutionValueToString value

        Failed reason ->
            reason
