module Direction exposing (..)

import Parser as P exposing ((|.), (|=), Parser, succeed)


type Direction
    = Downward Int
    | Upward Int
    | Forward Int
    | Backward Int
    | Unchanged


type alias Values =
    ( Int, Int )


calculateChange : List Direction -> Values
calculateChange dirs =
    List.map toValues dirs
        |> List.foldl addValues ( 0, 0 )


isDownward : Direction -> Bool
isDownward dir =
    case dir of
        Downward _ ->
            True

        _ ->
            False


isUpward : Direction -> Bool
isUpward dir =
    case dir of
        Upward _ ->
            True

        _ ->
            False


isForward : Direction -> Bool
isForward dir =
    case dir of
        Forward _ ->
            True

        _ ->
            False


toVerticalShifts : List Int -> List Direction
toVerticalShifts vals =
    case vals of
        h :: t ->
            List.map2 toVerticalShift vals t

        _ ->
            []


toVerticalShift : Int -> Int -> Direction
toVerticalShift a b =
    if b < a then
        Upward (a - b)

    else if b > a then
        Downward (b - a)

    else
        Unchanged


parseDirection : String -> Maybe Direction
parseDirection dirstring =
    P.run parseDirections dirstring
        |> Result.toMaybe


parseDirections : Parser Direction
parseDirections =
    P.oneOf
        [ parseUpward
        , parseDownward
        , parseForward
        , parseBackward
        ]


parseUpward : Parser Direction
parseUpward =
    succeed Upward
        |. P.keyword "up"
        |. P.spaces
        |= P.int


parseDownward : Parser Direction
parseDownward =
    succeed Downward
        |. P.keyword "down"
        |. P.spaces
        |= P.int


parseForward : Parser Direction
parseForward =
    succeed Forward
        |. P.keyword "forward"
        |. P.spaces
        |= P.int


parseBackward : Parser Direction
parseBackward =
    succeed Backward
        |. P.keyword "backward"
        |. P.spaces
        |= P.int


add : Direction -> Direction -> ( Int, Int )
add dir1 dir2 =
    addValues (toValues dir1) (toValues dir2)


addValues : Values -> Values -> Values
addValues ( x1, y1 ) ( x2, y2 ) =
    ( x1 + x2, y1 + y2 )


toValues : Direction -> Values
toValues dir =
    case dir of
        Upward y ->
            ( 0, -y )

        Downward y ->
            ( 0, y )

        Forward x ->
            ( x, 0 )

        Backward x ->
            ( -x, 0 )

        Unchanged ->
            ( 0, 0 )
