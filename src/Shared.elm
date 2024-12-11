module Shared exposing (..)


listToTuple : List a -> Maybe ( a, a )
listToTuple list =
    case list of
        [ x, y ] ->
            Just ( x, y )

        _ ->
            Nothing
