module Day5.Coordinates exposing (..)

import List.Extra exposing (gatherEquals)
import Parser as P exposing ((|.), (|=), Parser)


type alias Coordinates =
    ( Int, Int )


heatMap : List Coordinates -> List ( Coordinates, List Coordinates )
heatMap coords =
    let
        f ( coord, eqs ) =
            ( coord, 1 + List.length eqs )
    in
    gatherEquals coords



-- |> List.map f


parser : Parser Coordinates
parser =
    P.succeed Tuple.pair
        |. P.spaces
        |= P.int
        |. P.spaces
        |. P.symbol ","
        |. P.spaces
        |= P.int
        |. P.spaces



--


toString : ( Int, Int ) -> String
toString ( x, y ) =
    String.concat
        [ "("
        , String.fromInt x
        , ", "
        , String.fromInt y
        , ")"
        ]
