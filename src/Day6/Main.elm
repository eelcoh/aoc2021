module Day6.Main exposing (process)

import Day6.Fish as Fish exposing (Shoal)


process : String -> ( String, String )
process contents =
    let
        shoal =
            Fish.parse contents
    in
    ( part1 shoal, part2 shoal )


part1 : Shoal -> String
part1 fish =
    Fish.process 80 fish
        |> String.fromInt


part2 : Shoal -> String
part2 fish =
    Fish.process 256 fish
        |> String.fromInt
