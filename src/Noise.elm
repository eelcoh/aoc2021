module Noise exposing (Bit(..), Noise, bitsToString, bitsToValue, noiseValues, parseNoise, toCO2, toEpsilon, toGamma, toOxygen)

import List.Extra
import Parser as P exposing ((|.), (|=), Parser, oneOf, succeed)
import Value exposing (Values, addValues)
import ZipList exposing (ZipList)


type Noise
    = Noise Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit


type alias BitList =
    List Bit


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


toOxygen : List Noise -> Int
toOxygen noises =
    zip noises
        |> toOxygen_
        |> bitsToValue


toCO2 : List Noise -> Int
toCO2 noises =
    zip noises
        |> toCO2_
        |> bitsToValue


zip noises =
    List.map noiseToList noises
        |> List.map ZipList.fromList
        |> List.filterMap identity


toOxygen_ : List (ZipList Bit) -> List Bit
toOxygen_ noises =
    let
        keep =
            List.map ZipList.current noises
                |> List.partition ((==) Zero)
                |> criteria

        criteria ( zeroes, ones ) =
            if List.length ones >= List.length zeroes then
                One

            else
                Zero
    in
    case noises of
        x :: [] ->
            ZipList.toList x

        [] ->
            []

        _ ->
            List.filter (\x -> ZipList.current x == keep) noises
                |> List.map ZipList.forward
                |> toOxygen_


toCO2_ : List (ZipList Bit) -> List Bit
toCO2_ noises =
    let
        keep =
            List.map ZipList.current noises
                |> List.partition ((==) Zero)
                |> criteria

        criteria ( zeroes, ones ) =
            if List.length zeroes <= List.length ones then
                Zero

            else
                One
    in
    case noises of
        x :: [] ->
            ZipList.toList x

        [] ->
            []

        _ ->
            List.filter (\x -> ZipList.current x == keep) noises
                |> List.map ZipList.forward
                |> toCO2_


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
