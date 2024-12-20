module Shared exposing (..)

import List.Extra


listToTuple : List a -> Maybe ( a, a )
listToTuple list =
    case list of
        [ x, y ] ->
            Just ( x, y )

        _ ->
            Nothing


stringToCharMatrix : String -> List (List Char)
stringToCharMatrix input =
    input
        |> String.lines
        |> List.map String.toList
        |> List.filter (\l -> not (List.isEmpty l))


findTargetInMatrix : b -> List (List b) -> Maybe Position
findTargetInMatrix target matrix =
    matrix
        |> List.indexedMap Tuple.pair
        |> List.concatMap
            (\( rowIndex, row ) ->
                row
                    |> List.indexedMap Tuple.pair
                    |> List.filter (\( _, char ) -> char == target)
                    |> List.map (\( colIndex, _ ) -> ( rowIndex, colIndex ))
            )
        |> List.head


type alias Position =
    ( Int, Int )


type alias Field f =
    List (List f)


type alias Direction =
    ( Int, Int )


north : Direction
north =
    ( -1, 0 )


south : Direction
south =
    ( 1, 0 )


west : Direction
west =
    ( 0, -1 )


east : Direction
east =
    ( 0, 1 )


move : Direction -> Position -> Position
move ( dx, dy ) ( x, y ) =
    ( x + dx, y + dy )


in_bounds : Field a -> Position -> Bool
in_bounds field ( i, j ) =
    let
        i_len =
            List.length field

        j_len =
            List.head field
                |> Maybe.withDefault []
                |> List.length
    in
    (i <= i_len && i >= 0) && (j <= j_len && j >= 0)


getDataAtPosition : Field a -> Position -> Maybe a
getDataAtPosition field ( i, j ) =
    List.Extra.getAt i field
        |> Maybe.andThen
            (\row ->
                List.Extra.getAt j row
            )


replaceCharAtPosition : Position -> a -> Field a -> Field a
replaceCharAtPosition ( row, col ) replacement matrix =
    matrix
        |> List.Extra.updateAt row
            (\rowList ->
                rowList
                    |> List.Extra.updateAt col (\_ -> replacement)
            )
