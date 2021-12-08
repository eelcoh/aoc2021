module Day6.Main exposing (process)

import Day6.Fish as Fish exposing (Fish)


process : String -> ( String, String )
process contents =
    let
        fish =
            Fish.parse contents

        -- String.lines contents
        --     |> Vector.parse
    in
    ( part1 fish, part2 fish )


part1 : Fish -> String
part1 fish =
    Fish.roll 80 fish
        -- |> List.length
        |> String.fromInt


part2 : Fish -> String
part2 fish =
    Fish.roll 256 fish
        -- |> List.length
        |> String.fromInt
