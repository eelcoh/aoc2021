module Noise exposing (noiseValues, parseNoise)

import List.Extra
import Parser as P exposing ((|.), (|=), Parser, oneOf, succeed)
import Value exposing (Values, addValues)


type Noise
    = Noise Bit Bit Bit Bit Bit


type Bit
    = Zero
    | One


bitToInt : Bit -> Int
bitToInt b =
    case b of
        Zero ->
            0

        One ->
            1


noiseValue : Noise -> Bit
noiseValue (Noise a b c d e) =
    List.map bitToInt [ a, b, c, d, e ]
        |> List.sum
        |> (\x ->
                if x > 2 then
                    One

                else
                    Zero
           )


noiseValues : List Noise -> ( Int, Int )
noiseValues noises =
    let
        toValues n =
            case n of
                One ->
                    ( 0, 1 )

                Zero ->
                    ( 1, 0 )
    in
    List.map noiseValue noises
        |> List.map toValues
        |> Value.sum



-- calc
-- parsers


parseNoise : String -> Maybe Noise
parseNoise noiseString =
    runParseNoise noiseString |> Result.toMaybe


runParseNoise : String -> Result (List P.DeadEnd) Noise
runParseNoise noiseString =
    P.run parseNoiseString noiseString


parseNoiseString : Parser Noise
parseNoiseString =
    succeed Noise
        |= bit
        |= bit
        |= bit
        |= bit
        |= bit


bit : Parser Bit
bit =
    oneOf
        [ zero, one ]


one : Parser Bit
one =
    succeed One
        |. P.symbol "1"


zero : Parser Bit
zero =
    succeed Zero
        |. P.symbol "0"
