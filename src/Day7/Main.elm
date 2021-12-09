module Day7.Main exposing (process)

import Day7.Crab as Crab exposing (Swarm)


process : String -> ( String, String )
process contents =
    let
        swarm =
            Crab.parse contents
    in
    ( part1 swarm, part2 swarm )


part1 : Swarm -> String
part1 swarm =
    Crab.process1 swarm
        |> String.fromInt


part2 : Swarm -> String
part2 swarm =
    Crab.process2 swarm
