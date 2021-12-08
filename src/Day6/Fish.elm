module Day6.Fish exposing (..)

import Dict exposing (Dict)
import List.Extra


type alias Fish =
    List Int


type alias OffspringTable =
    Dict Int Int


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



{-
   We can determine the offspring size for every combination of
   * the spawn period of a single fish
   * the number of days

   such that you get
       f days spawn -> value

   the set of (days, spawn) is limited to days * spawn.
   but in the end, we only need the spawn table for days. Not for days - 1.
   that means the table has a constant size of 9

   if we have the set for (n, spawn) we should easily be able to determine
   the set for (n+1, spawn - 1)

    so let's say we start with
        days = 1, spawn [0..8]
            if spawn == 0 then
                size = 2
            else
                size = 1

    [ 0 -> 2
    , 1 -> 1
    , 2 -> 1
    , 3 -> 1
    , 4 -> 1
    , 5 -> 1
    , 6 -> 1
    , 7 -> 1
    , 8 -> 1
    ]

    then  days = 2, spawn [0..8], table
        [ 0 -> 6 -> 1 + 0 -> 8 -> 1 = -> 2
        , 1 -> 0 -> 2
        , 2 -> 1 -> 1
        , ...
    then  days = 2, spawn [0..8], table
        [ 0 -> 6 -> 1 + 0 -> 8 -> 1 = -> 2
        , 1 -> 0 -> 2
        , 2 -> 1 -> 2
        , 3 -> 2 -> 2...

    then  days = 2, spawn [0..8], table
        [ 0 -> 6 -> 2 + 0 -> 8 -> 1 = -> 3
        , 1 -> 0 -> 2
        , 2 -> 1 -> 2
        , 3 -> 2 -> 2...

    so
        f table spawns =
            roll spawns
            |> map (getValue table)



   and make a lookup table for that

   then create the lookup for days = 2, spawn = [1..8]

    so first we create the spawn table for day 1
    then we transform that into create the spawn table for day 2
    then we transform that into to create the spawn table for day n+1
    until we have created the table for days requested


-}


rollover : Int -> OffspringTable -> OffspringTable
rollover days table =
    let
        f k v =
            if k == 0 then
                Maybe.map2 (+) (Dict.get 6 table) (Dict.get 8 table)
                    |> Maybe.map (Tuple.pair k)

            else
                Dict.get (k - 1) table
                    |> Maybe.map (Tuple.pair k)
    in
    if days == 0 then
        table

    else
        Dict.toList table
            |> List.filterMap (\( k, v ) -> f k v)
            |> Dict.fromList
            |> rollover (days - 1)


roll : Int -> Fish -> Int
roll days fish =
    let
        initTable =
            Dict.fromList
                [ ( 0, 1 )
                , ( 1, 1 )
                , ( 2, 1 )
                , ( 3, 1 )
                , ( 4, 1 )
                , ( 5, 1 )
                , ( 6, 1 )
                , ( 7, 1 )
                , ( 8, 1 )
                ]

        table =
            rollover days initTable
    in
    List.filterMap (\f -> Dict.get f table) fish
        |> List.sum



{- NAIVE will stack overflow -}


rollNaive : Int -> Fish -> Fish
rollNaive days fish =
    if days <= 0 then
        fish

    else
        rollNaive (days - 1) (rollOnceNaive fish)


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
