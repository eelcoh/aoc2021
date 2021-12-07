module Day5.Vector exposing (..)

import Day5.Coordinates as Coordinates exposing (Coordinates)
import Parser as P exposing ((|.), (|=), Parser)


type alias Vector =
    ( Coordinates, Coordinates )


isHorizontal ( ( x1, _ ), ( x2, _ ) ) =
    x1 == x2


isVertical ( ( _, y1 ), ( _, y2 ) ) =
    y1 == y2


toCoordinates : Vector -> List Coordinates
toCoordinates ( ( x1, y1 ), ( x2, y2 ) ) =
    if x1 == x2 then
        List.range (min y1 y2) (max y1 y2)
            |> List.map (\y -> ( x1, y ))

    else if y1 == y2 then
        List.range (min x1 x2) (max x1 x2)
            |> List.map (\x -> ( x, y1 ))

    else
        []



-- parsing


parse : List String -> List Vector
parse vectorStrings =
    List.filterMap runParser vectorStrings


runParser : String -> Maybe Vector
runParser vectorString =
    P.run parser vectorString
        |> Result.toMaybe


parser : Parser Vector
parser =
    P.succeed Tuple.pair
        |= Coordinates.parser
        |. P.spaces
        |. P.symbol "->"
        |. P.spaces
        |= Coordinates.parser



-- print


toString : Vector -> String
toString ( c1, c2 ) =
    String.concat
        [ Coordinates.toString c1
        , " -> "
        , Coordinates.toString c2
        ]
