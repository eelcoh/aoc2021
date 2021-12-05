module Noise exposing (bitsToString, bitsToValue, noiseValues, parseNoise, toEpsilon, toGamma)

import List.Extra
import Parser as P exposing ((|.), (|=), Parser, oneOf, succeed)
import Value exposing (Values, addValues)


type Noise
    = Noise Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit


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
noiseValue (Noise a b c d e f g h i j k l) =
    List.map bitToInt [ a, b, c, d, e, f, g, h, i, j, k, l ]
        |> List.sum
        |> (\x ->
                if x > 2 then
                    One

                else
                    Zero
           )


noiseToList : Noise -> List Bit
noiseToList (Noise a b c d e f g h i j k l) =
    [ a, b, c, d, e, f, g, h, i, j, k, l ]


toGamma : List Noise -> List Bit
toGamma noises =
    List.map noiseToList noises
        |> List.Extra.transpose
        |> List.map toGamma_


toEpsilon : List Bit -> List Bit
toEpsilon bits =
    let
        inverse bit_ =
            case bit_ of
                One ->
                    Zero

                Zero ->
                    One
    in
    List.map inverse bits


toGamma_ : List Bit -> Bit
toGamma_ bits =
    let
        toValues n =
            case n of
                One ->
                    ( 0, 1 )

                Zero ->
                    ( 1, 0 )

        toBit ( a, b ) =
            if a > b then
                Zero

            else
                One
    in
    List.map toValues bits
        |> Value.sum
        |> toBit


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
        |= bit
        |= bit
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


bitsToValue : List Bit -> Int
bitsToValue bits =
    let
        toValue_ bit_ bitValue =
            case bit_ of
                One ->
                    bitValue

                Zero ->
                    0
    in
    List.range 0 (List.length bits)
        |> List.map (\v -> 2 ^ v)
        |> List.map2 toValue_ (List.reverse bits)
        |> List.sum


bitsToString : List Bit -> String
bitsToString bits =
    let
        toValue_ bit_ =
            case bit_ of
                One ->
                    "1"

                Zero ->
                    "0"
    in
    List.map toValue_ bits
        |> String.concat
