module Day2.Direction exposing (..)

import Day2.Direction.Parser as P exposing (..)
import Day2.Direction.Types exposing (Direction(..))
import Day3.Value exposing (Values, addValues)
import List.Extra


calculateChange : List Direction -> Int
calculateChange dirs =
    List.map toValues dirs
        |> List.foldl addValues ( 0, 0 )
        |> (\( a, b ) -> a * b)


calculateWithAim : List Direction -> Int
calculateWithAim dirs =
    calculateWithAim_ ( 0, ( 0, 0 ) ) dirs
        |> (\( a, b ) -> a * b)


calculateWithAim_ : ( Int, Values ) -> List Direction -> Values
calculateWithAim_ ( aim, vals ) dirs =
    let
        ( aim_, vals_ ) =
            List.foldl toValuesAim ( aim, vals ) dirs
    in
    vals_


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



-- forward 5 adds 5 to your horizontal position, a total of 5. Because your aim is 0, your depth does not change.
-- -> (0, (5, 0))
-- down 5 adds 5 to your aim, resulting in a value of 5.
-- -> (5, (5, 0))
-- forward 8 adds 8 to your horizontal position, a total of 13. Because your aim is 5, your depth increases by 8*5=40.
-- -> (5, (13, 40))
-- up 3 decreases your aim by 3, resulting in a value of 2.
-- -> (2, (13, 40))
-- down 8 adds 8 to your aim, resulting in a value of 10.
-- -> (10, (13, 40))
-- forward 2 adds 2 to your horizontal position, a total of 15. Because your aim is 10, your depth increases by 2*10=20 to a total of 60.
-- -> (10, (15, 60))


toValuesAim : Direction -> ( Int, Values ) -> ( Int, Values )
toValuesAim dir ( aim, ( x, y ) ) =
    case dir of
        Upward y_ ->
            ( aim - y_, ( x, y ) )

        Downward y_ ->
            ( aim + y_, ( x, y ) )

        Forward x_ ->
            ( aim, ( x + x_, y + (aim * x_) ) )

        Backward x_ ->
            ( aim, ( x - x_, y - (aim * x_) ) )

        Unchanged ->
            ( aim, ( x, y ) )
