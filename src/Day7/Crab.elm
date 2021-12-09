module Day7.Crab exposing (..)

import Dict exposing (Dict)
import Maybe exposing (withDefault)
import Stat exposing (median)


type alias Crab =
    Int


type alias Swarm =
    List Crab


process1 : Swarm -> Int
process1 swarm =
    let
        medianValue =
            List.map Basics.toFloat swarm
                |> median
                |> Maybe.map Basics.round

        distance =
            \a -> (-) a >> abs

        distances =
            List.map distance swarm
                |> List.filterMap (\f -> Maybe.map f medianValue)
                |> List.sum
    in
    distances



-- List.map ((-) medianValue >> Basics.abs) swarm
--     |> List.sum
-- parse


process2 : Swarm -> String
process2 swarm =
    let
        minimum =
            Maybe.withDefault 0 (List.minimum swarm)

        maximum =
            Maybe.withDefault 1000 (List.maximum swarm)

        potentials =
            List.range minimum maximum

        initialTable : Dict Int Int
        initialTable =
            List.map (\a -> Tuple.pair a 0) potentials
                |> Dict.fromList

        distance a b =
            List.range 0 (abs (b - a))
                |> List.sum

        addDistance : Int -> Dict Int Int -> Dict Int Int
        addDistance i table =
            Dict.map (\k v -> v + distance i k) table

        distances =
            List.foldl addDistance initialTable swarm
    in
    distances
        |> Dict.values
        |> List.minimum
        |> Maybe.map String.fromInt
        |> Maybe.withDefault "whoops"


parse : String -> Swarm
parse contents =
    String.lines contents
        |> List.head
        |> Maybe.map parse_
        |> Maybe.withDefault []


parse_ : String -> Swarm
parse_ contents =
    String.split "," contents
        |> List.filterMap String.toInt
