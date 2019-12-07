module Solution exposing (Solution(..), SolutionValue, fromInt, solutionValueToString)


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


solutionValueToString : SolutionValue -> String
solutionValueToString val =
    case val of
        StringVal s ->
            s

        IntVal i ->
            String.fromInt i
