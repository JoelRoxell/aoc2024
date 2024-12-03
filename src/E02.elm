module E02 exposing (e02a, e02b)


e02a : String -> number
e02a data =
    parse data
        |> List.map
            (\report -> isValid Nothing report)
        |> sum


e02b : String -> number
e02b data =
    parse data
        |> List.map
            (\report ->
                List.any (isValid Nothing) (generateRemovalPermutations report)
            )
        |> sum


sum : List Bool -> number
sum =
    List.foldl
        (\a acc ->
            if a then
                acc + 1

            else
                acc
        )
        0


type Direction
    = Up
    | Down
    | None


isValid : Maybe Direction -> List Int -> Bool
isValid direction list =
    case list of
        current :: next :: rest ->
            let
                ( ok, dir ) =
                    travel_direction current next direction
            in
            if ok then
                isValid (Just dir) (next :: rest)

            else
                False

        _ ->
            True


generateRemovalPermutations : List a -> List (List a)
generateRemovalPermutations levels =
    List.indexedMap (\i _ -> removeAt i levels) levels


removeAt : Int -> List a -> List a
removeAt index list =
    List.take index list ++ List.drop (index + 1) list


travel_direction : number -> number -> Maybe Direction -> ( Bool, Direction )
travel_direction current next prevDirection =
    let
        travel =
            current - next

        direction =
            if travel == 0 then
                None

            else if travel > 0 then
                Up

            else
                Down
    in
    ( Basics.abs travel <= 3 && direction == Maybe.withDefault direction prevDirection, direction )


parse : String -> List (List Int)
parse str =
    str
        |> String.split "\n"
        |> List.map
            (\line -> String.split " " line)
        |> List.map
            (\row -> List.filterMap (\a -> String.toInt a) row)
        |> List.filter (\b -> not (List.isEmpty b))
