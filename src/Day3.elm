module Day3 exposing (process)

import List.Extra exposing (scanl1)
import Noise exposing (..)



-- → elm-cli run src/Day3.elm inputs/day3.txt
-- gamma : 110000111111 - 3135
-- epsilon : 001111000000 - 960
-- total : 3009600


process : String -> ( String, String )
process contents =
    let
        noise =
            String.lines contents
                |> List.filterMap parseNoise
    in
    ( part1 noise, part2 noise )


part1 : List Noise -> String
part1 noise =
    let
        gamma =
            toGamma noise

        epsilon =
            toEpsilon gamma

        gammaValue =
            bitsToValue gamma

        epsilonValue =
            bitsToValue epsilon
    in
    String.fromInt (epsilonValue * gammaValue)


part2 _ =
    "not implemented"
