module Direction exposing (..)

import Direction.Parser as P exposing (..)
import Direction.Types exposing (Direction(..), Values)


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
    P.parseDirection dirstring |> Result.toMaybe


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
