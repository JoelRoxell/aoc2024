module E05 exposing (f1, f2)

import List.Extra
import Shared exposing (listToTuple)


f1 : String -> Int
f1 data =
    let
        ( rules, input ) =
            parse data
    in
    findMiddlePages rules input
        |> List.filterMap identity
        |> List.sum


f2 : String -> Int
f2 data =
    let
        ( rules, input ) =
            parse data

        faulty_rows =
            List.foldl
                (\page acc ->
                    if not (isOrdered rules page) then
                        page :: acc

                    else
                        acc
                )
                []
                input
    in
    faulty_rows
        |> List.map (fixUpdate rules)
        |> List.filterMap findMiddle
        |> List.sum


isOrdered : List ( Int, Int ) -> List Int -> Bool
isOrdered rules page =
    List.all
        (\( x, y ) ->
            let
                indexX =
                    List.indexedMap Tuple.pair page
                        |> List.filter (\( _, v ) -> v == x)
                        |> List.head
                        |> Maybe.map Tuple.first

                indexY =
                    List.indexedMap Tuple.pair page
                        |> List.filter (\( _, v ) -> v == y)
                        |> List.head
                        |> Maybe.map Tuple.first
            in
            case ( indexX, indexY ) of
                ( Just ix, Just iy ) ->
                    ix < iy

                _ ->
                    True
        )
        rules


findMiddle : List Int -> Maybe Int
findMiddle page =
    let
        len =
            List.length page
    in
    if len == 0 then
        Nothing

    else
        page
            |> List.drop ((len - 1) // 2)
            |> List.head


findMiddlePages : List ( Int, Int ) -> List (List Int) -> List (Maybe Int)
findMiddlePages rules updates =
    updates
        |> List.filter (isOrdered rules)
        |> List.map findMiddle


sortPages : List ( Int, Int ) -> List Int -> List Int
sortPages rules pages =
    List.sortWith (\a b -> comparePages rules a b) pages


comparePages : List ( Int, Int ) -> Int -> Int -> Order
comparePages rules a b =
    if List.any (\( x, y ) -> x == a && y == b) rules then
        LT

    else if List.any (\( x, y ) -> x == b && y == a) rules then
        GT

    else
        EQ


fixUpdate : List ( Int, Int ) -> List Int -> List Int
fixUpdate rules page =
    sortPages rules page


parse : String -> ( List ( Int, Int ), List (List Int) )
parse data =
    let
        chunks =
            String.split "\n\n" data

        rules =
            List.Extra.getAt 0 chunks
                |> Maybe.withDefault ""
                |> String.split "\n"
                |> List.map
                    (\row ->
                        String.split "|" row
                            |> List.filterMap String.toInt
                            |> listToTuple
                    )
                |> List.filterMap identity

        batch =
            List.Extra.getAt 1 chunks
                |> Maybe.withDefault ""
                |> String.split "\n"
                |> List.map
                    (\row ->
                        String.split "," row
                            |> List.filterMap String.toInt
                    )
    in
    ( rules, batch )
