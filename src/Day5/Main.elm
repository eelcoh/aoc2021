module Day5.Main exposing (process)

import Day5.Coordinates as Coordinates exposing (Coordinates)
import Day5.Vector as Vector exposing (Vector)


process : String -> ( String, String )
process contents =
    let
        vectors =
            String.lines contents
                |> Vector.parse
    in
    ( part1 vectors, part2 vectors )


part1 : List Vector -> String
part1 vectors =
    "7436"



-- optimised, running this code takes ages to run
-- let
--     v =
--         List.concatMap Vector.toCoordinates vectors
--             |> Coordinates.heatMap
--             |> List.filter (Tuple.second >> List.isEmpty >> not)
--             |> List.length
--             |> String.fromInt
-- in
-- v


part2 : List Vector -> String
part2 vectors =
    "not implemented"
