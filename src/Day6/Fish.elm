module Day6.Fish exposing (..)

import Dict exposing (Dict)
import List.Extra


type alias Fish =
    List Int


parse : String -> Fish
parse contents =
    String.lines (Debug.log "contents" contents)
        |> List.head
        |> Maybe.map parse_
        |> Maybe.withDefault []


parse_ : String -> Fish
parse_ contents =
    String.split "," contents
        |> List.filterMap String.toInt


roll : Int -> Fish -> Int
roll numRolls fish =
    let
        table : Dict.Dict Int Int
        table =
            List.map
                (\i -> Tuple.pair i (toValue i))
                (List.range 1 8)
                |> Dict.fromList

        toValue i =
            roll_ numRolls (List.singleton i) |> List.length

        getValue t v =
            Maybe.withDefault (roll_ numRolls (List.singleton v) |> List.length) (Dict.get v t)
    in
    List.map (getValue table) fish
        |> List.sum


roll_ : Int -> Fish -> Fish
roll_ numRolls fish =
    if numRolls <= 0 then
        fish

    else
        roll_ (numRolls - 1) (rollOnceNaive fish)


rollOnceNaive : Fish -> Fish
rollOnceNaive fish =
    case fish of
        x :: rest ->
            let
                newX =
                    x - 1
            in
            if newX < 0 then
                6 :: 8 :: rollOnceNaive rest

            else
                newX :: rollOnceNaive rest

        [] ->
            []
