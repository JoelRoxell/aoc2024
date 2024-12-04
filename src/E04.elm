module E04 exposing (e04a, e04b)

import List.Extra


e04a : String -> Int
e04a data =
    let
        search_term =
            "XMAS"

        field =
            parse data

        directions =
            [ Left, Right, Up, Down, UpLeft, DownLeft, UpRight, DownRight ]

        startingpoints =
            List.concat
                (List.indexedMap
                    (\i row ->
                        List.indexedMap
                            (\j char ->
                                if char == term_start search_term then
                                    Just ( i, j )

                                else
                                    Nothing
                            )
                            row
                            |> List.filterMap identity
                    )
                    field
                )
    in
    List.concatMap
        (\pos ->
            List.map (validate_term search_term)
                (List.map
                    (\dir ->
                        take_direction field pos (String.length search_term) dir
                    )
                    directions
                    |> List.map (String.join "")
                )
        )
        startingpoints
        |> List.sum


e04b : String -> Int
e04b data =
    let
        target =
            "A"

        field =
            parse data

        startingpoints =
            List.concat
                (List.indexedMap
                    (\i row ->
                        List.indexedMap
                            (\j char ->
                                if char == term_start target then
                                    Just ( i, j )

                                else
                                    Nothing
                            )
                            row
                            |> List.filterMap identity
                    )
                    field
                )
    in
    List.map
        (\pos ->
            List.any identity
                [ List.all
                    identity
                    [ take_direction field pos 2 UpLeft |> validate_tail "M"
                    , take_direction field pos 2 UpRight |> validate_tail "M"
                    , take_direction field pos 2 DownLeft |> validate_tail "S"
                    , take_direction field pos 2 DownRight |> validate_tail "S"
                    ]
                , List.all
                    identity
                    [ take_direction field pos 2 UpLeft |> validate_tail "M"
                    , take_direction field pos 2 UpRight |> validate_tail "S"
                    , take_direction field pos 2 DownLeft |> validate_tail "M"
                    , take_direction field pos 2 DownRight |> validate_tail "S"
                    ]
                , List.all
                    identity
                    [ take_direction field pos 2 UpLeft |> validate_tail "S"
                    , take_direction field pos 2 UpRight |> validate_tail "S"
                    , take_direction field pos 2 DownLeft |> validate_tail "M"
                    , take_direction field pos 2 DownRight |> validate_tail "M"
                    ]
                , List.all
                    identity
                    [ take_direction field pos 2 UpLeft |> validate_tail "S"
                    , take_direction field pos 2 UpRight |> validate_tail "M"
                    , take_direction field pos 2 DownLeft |> validate_tail "S"
                    , take_direction field pos 2 DownRight |> validate_tail "M"
                    ]
                ]
        )
        startingpoints
        |> List.map
            (\a ->
                if a == True then
                    1

                else
                    0
            )
        |> List.sum


type Direction
    = Left
    | Right
    | Up
    | Down
    | UpRight
    | DownRight
    | UpLeft
    | DownLeft


validate_tail : String -> List String -> Bool
validate_tail match list =
    List.reverse list
        |> List.head
        |> Maybe.withDefault ""
        |> (==) match


validate_term : String -> String -> Int
validate_term s match =
    if s == match then
        1

    else
        0


term_start : String -> String
term_start search_term =
    List.head (String.split "" search_term) |> Maybe.withDefault ""


parse : String -> List (List String)
parse data =
    data
        |> String.split "\n"
        |> List.filter (\s -> String.length s > 0)
        |> List.map (String.split "")


take_direction : List (List String) -> ( Int, Int ) -> Int -> Direction -> List String
take_direction field ( i, j ) term_len direction =
    let
        getRow idx =
            List.Extra.getAt idx field |> Maybe.withDefault []

        getColumn colIdx =
            List.map
                (\row ->
                    List.Extra.getAt colIdx row |> Maybe.withDefault ""
                )
                field

        diagonal step rowIdx colIdx =
            if rowIdx < 0 || colIdx < 0 then
                []

            else
                case List.Extra.getAt rowIdx field of
                    Just row ->
                        case List.Extra.getAt colIdx row of
                            Just value ->
                                value
                                    :: diagonal step
                                        (rowIdx + Tuple.first step)
                                        (colIdx + Tuple.second step)

                            Nothing ->
                                []

                    Nothing ->
                        []
    in
    case direction of
        Left ->
            let
                row =
                    getRow i
            in
            List.take (j + 1) row
                |> List.drop (j + 1 - term_len)
                |> List.reverse

        Right ->
            let
                row =
                    getRow i
            in
            List.drop j row
                |> List.take term_len

        Down ->
            let
                col =
                    getColumn j
            in
            List.drop i col
                |> List.take term_len

        Up ->
            let
                col =
                    getColumn j
            in
            List.take (i + 1) col
                |> List.drop (i + 1 - term_len)
                |> List.reverse

        DownRight ->
            diagonal ( 1, 1 ) i j |> List.take term_len

        UpLeft ->
            diagonal ( -1, -1 ) i j |> List.take term_len

        DownLeft ->
            diagonal ( 1, -1 ) i j |> List.take term_len

        UpRight ->
            diagonal ( -1, 1 ) i j |> List.take term_len
