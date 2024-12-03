module E03 exposing (e03a, e03b)

import Regex exposing (find, fromString)


e03a : String -> Int
e03a data =
    data
        |> findLangMatchers
        |> List.map parseLangToken
        |> List.filterMap identity
        |> List.map
            (\row ->
                case row of
                    Mul a b ->
                        a * b

                    _ ->
                        0
            )
        |> List.sum


e03b : String -> Int
e03b data =
    data
        |> findLangMatchers
        |> List.map parseLangToken
        |> List.filterMap identity
        |> List.foldl
            (\token acc ->
                case token of
                    Mul a b ->
                        if acc.do then
                            { acc | sum = acc.sum + (a * b) }

                        else
                            acc

                    Do ->
                        { acc | do = True }

                    Dont ->
                        { acc | do = False }
            )
            { sum = 0, do = True }
        |> .sum


type Token
    = Mul Int Int
    | Dont
    | Do


parseLangToken : String -> Maybe Token
parseLangToken str =
    if String.contains "mul" str then
        findPair str
            |> toTuple
            |> Maybe.map (\( a, b ) -> Mul a b)

    else if String.contains "don't" str then
        Just Dont

    else
        Just Do


findLangMatchers : String -> List String
findLangMatchers str =
    let
        pattern =
            Maybe.withDefault Regex.never <|
                fromString "mul\\(\\d+,\\s*\\d+\\)|do\\(\\)|don't\\(\\)"
    in
    List.map (\match -> match.match) (find pattern str)


findPair : String -> List Int
findPair str =
    let
        pattern =
            Maybe.withDefault Regex.never <|
                fromString "\\d+,\\d+"
    in
    find pattern str
        |> List.map (\match -> match.match)
        |> List.concatMap (String.split ",")
        |> List.map String.toInt
        |> List.map (\a -> Maybe.withDefault 0 a)


toTuple : List Int -> Maybe ( Int, Int )
toTuple list =
    case list of
        [ a, b ] ->
            Just ( a, b )

        _ ->
            Nothing
