module E06 exposing (f1)

import Set exposing (Set)
import Shared exposing (Direction, Position, getDataAtPosition, replaceCharAtPosition)


f1 : String -> ( Int, Int )
f1 data =
    let
        field =
            parse data

        res =
            move field (findGuard field) Set.empty True 0
    in
    case res of
        Terminated { visited, stuck_counter } ->
            ( visited
                |> Set.toList
                |> List.map (\( pos, _ ) -> pos)
                |> Set.fromList
                |> Set.size
            , stuck_counter
            )

        Stuck _ ->
            ( -1, -1 )


parse : String -> Field
parse data =
    Shared.stringToCharMatrix data


type LoopResult a
    = Stuck a
    | Terminated a


type alias Output =
    { field : Field
    , guard : Guard
    , visited : Set ( Position, Direction )
    , stuck_counter : Int
    }


move :
    Field
    -> Guard
    -> Set ( Position, Direction )
    -> Bool
    -> Int
    -> LoopResult Output
move field guard visited insert_obsticle stuck_counter =
    let
        next_position =
            Shared.move
                guard.direction
                guard.position

        out_of_bounds =
            Shared.in_bounds field next_position |> not

        data_at_position =
            Shared.getDataAtPosition field next_position

        should_rotate =
            data_at_position
                |> Maybe.andThen
                    (\c ->
                        Just (c == '#')
                    )
                |> Maybe.withDefault False

        next_guard_position =
            if should_rotate then
                { guard | direction = turnGuard guard.direction }

            else
                { guard | position = next_position }

        visited_state : Set ( Position, Direction )
        visited_state =
            Set.insert ( guard.position, guard.direction ) visited
    in
    if Set.member ( next_guard_position.position, next_guard_position.direction ) visited then
        Stuck (Output field guard visited stuck_counter)

    else if out_of_bounds then
        Terminated (Output field guard visited stuck_counter)

    else
        let
            obsticle_variation_result =
                if insert_obsticle then
                    Just
                        (move
                            (placeObsticle next_guard_position.position field)
                            guard
                            visited_state
                            False
                            0
                        )

                else
                    Nothing

            stuck =
                Maybe.map
                    (\b ->
                        case b of
                            Stuck _ ->
                                1

                            Terminated _ ->
                                0
                    )
                    obsticle_variation_result
                    |> Maybe.withDefault 0
        in
        move field next_guard_position visited_state insert_obsticle (stuck_counter + stuck)


placeObsticle : Position -> Field -> Field
placeObsticle p field =
    let
        nextPosData =
            getDataAtPosition field p |> Maybe.withDefault '#'
    in
    if nextPosData == '#' then
        field

    else
        replaceCharAtPosition p '#' field


findGuard : Field -> Guard
findGuard field =
    { position =
        Shared.findTargetInMatrix '^' field
            |> Maybe.withDefault ( 0, 0 )
    , direction = Shared.north
    }


turnGuard : Direction -> Direction
turnGuard direction =
    if direction == Shared.north then
        Shared.east

    else if direction == Shared.east then
        Shared.south

    else if direction == Shared.south then
        Shared.west

    else
        Shared.north


type alias Guard =
    { position : Shared.Position
    , direction : Shared.Direction
    }


type alias Field =
    Shared.Field Char
