module Day1 exposing (process)

import Direction exposing (..)
import Direction.Types exposing (Direction)
import List.Extra


process : String -> ( String, String )
process filecontents =
    let
        input =
            String.lines filecontents
                |> List.filterMap String.toInt
    in
    ( part1 input, part2 input )


part1 : List Int -> String
part1 inputs =
    toVerticalShifts inputs
        |> countValues
        |> Tuple.first
        |> String.fromInt


part2 : List Int -> String
part2 inputs =
    List.Extra.groupsOfWithStep 3 1 inputs
        |> List.map List.sum
        |> toVerticalShifts
        |> countValues
        |> Tuple.first
        |> String.fromInt


countValues : List Direction -> ( Int, Int )
countValues dirs =
    ( List.Extra.count isDownward dirs, List.Extra.count isUpward dirs )
