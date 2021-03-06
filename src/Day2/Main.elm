module Day2.Main exposing (process)

import Day2.Direction exposing (calculateChange, calculateWithAim, parseDirection)
import Day2.Direction.Types exposing (Direction)


process : String -> ( String, String )
process filecontents =
    let
        input =
            String.lines filecontents
                |> List.filterMap parseDirection
    in
    ( part1 input, part2 input )


part1 : List Direction -> String
part1 dirs =
    calculateChange dirs
        |> String.fromInt


part2 : List Direction -> String
part2 dirs =
    calculateWithAim dirs
        |> String.fromInt
