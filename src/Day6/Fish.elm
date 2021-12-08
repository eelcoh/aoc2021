module Day6.Fish exposing (..)


type alias Fish =
    Int


type alias Shoal =
    List Fish


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


calculateTable : Int -> SpawnTable -> SpawnTable
calculateTable days table =
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
            |> calculateTable (days - 1)


process : Int -> Shoal -> Int
process days fish =
    let
        table =
            calculateTable days initTable
    in
    List.filterMap (lookup table) fish
        |> List.sum



{- NAIVE will stack overflow -}


processNaive : Int -> Shoal -> Shoal
processNaive days fish =
    if days <= 0 then
        fish

    else
        processNaive (days - 1) (processOneNaive fish)


processOneNaive : Shoal -> Shoal
processOneNaive fish =
    case fish of
        [] ->
            []

        x :: rest ->
            let
                newX =
                    x - 1
            in
            if newX < 0 then
                6 :: 8 :: processOneNaive rest

            else
                newX :: processOneNaive rest



-- parse


parse : String -> Shoal
parse contents =
    String.lines contents
        |> List.head
        |> Maybe.map parse_
        |> Maybe.withDefault []


parse_ : String -> Shoal
parse_ contents =
    String.split "," contents
        |> List.filterMap String.toInt
