module E01 exposing (eo1a, eo1b)

import Dict
import List.Extra


eo1a : String -> Int
eo1a data =
    parse data
        |> List.map (\( a, b ) -> Basics.abs (a - b))
        |> List.sum


eo1b : String -> Int
eo1b data =
    let
        input =
            parse data

        right_count =
            List.foldl
                (\( _, b ) acc ->
                    Dict.update b (\num -> Just (Maybe.withDefault 0 num + 1)) acc
                )
                Dict.empty
                input
    in
    List.filterMap
        (\( a, _ ) -> Maybe.map (\b -> b * a) (Dict.get a right_count))
        input
        |> List.sum


parse : String -> List ( Int, Int )
parse data =
    let
        res =
            data
                |> String.split "\n"
                |> List.map
                    (String.split " ")
                |> List.map
                    (List.filter (not << String.isEmpty))
                |> List.filter
                    (not << List.isEmpty)
                |> List.map
                    (List.map (String.toInt >> Maybe.withDefault 0))
                |> List.map
                    (\a ->
                        ( List.Extra.getAt 0 a |> Maybe.withDefault 0
                        , List.Extra.getAt 1 a |> Maybe.withDefault 0
                        )
                    )

        ( left, right ) =
            List.foldl
                (\( a, b ) ( acc1, acc2 ) -> ( a :: acc1, b :: acc2 ))
                ( [], [] )
                res
    in
    List.Extra.zip
        (List.sort left)
        (List.sort right)
