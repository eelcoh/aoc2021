module Day6.Fish exposing (..)


type alias Fish =
    List Int


type alias SpawnTable =
    List ( Int, Int )


lookup : SpawnTable -> Int -> Maybe Int
lookup t i0 =
    case t of
        ( ix, v ) :: rest ->
            if ix == i0 then
                Just v

            else
                lookup rest i0

        [] ->
            Nothing


initTable : SpawnTable
initTable =
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



-- See readme.md for explanation


rollover : Int -> SpawnTable -> SpawnTable
rollover days table =
    let
        f ( k, _ ) =
            if k == 0 then
                Maybe.map2 (+) (lookup table 6) (lookup table 8)
                    |> Maybe.map (Tuple.pair k)

            else
                lookup table (k - 1)
                    |> Maybe.map (Tuple.pair k)
    in
    if days == 0 then
        table

    else
        table
            |> List.filterMap f
            |> rollover (days - 1)


roll : Int -> Fish -> Int
roll days fish =
    let
        table =
            rollover days initTable
    in
    List.filterMap (lookup table) fish
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
        [] ->
            []

        x :: rest ->
            let
                newX =
                    x - 1
            in
            if newX < 0 then
                6 :: 8 :: rollOnceNaive rest

            else
                newX :: rollOnceNaive rest



-- parse


parse : String -> Fish
parse contents =
    String.lines contents
        |> List.head
        |> Maybe.map parse_
        |> Maybe.withDefault []


parse_ : String -> Fish
parse_ contents =
    String.split "," contents
        |> List.filterMap String.toInt
