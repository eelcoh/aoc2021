module Day2.Direction.Parser exposing (..)

import Day2.Direction.Types exposing (Direction(..))
import Parser as P exposing ((|.), (|=), Parser, succeed)


parseDirection : String -> Result (List P.DeadEnd) Direction
parseDirection dirstring =
    P.run parseDirections dirstring


parseDirections : Parser Direction
parseDirections =
    P.oneOf
        [ parseUpward
        , parseDownward
        , parseForward
        , parseBackward
        ]


parseUpward : Parser Direction
parseUpward =
    succeed Upward
        |. P.keyword "up"
        |. P.spaces
        |= P.int


parseDownward : Parser Direction
parseDownward =
    succeed Downward
        |. P.keyword "down"
        |. P.spaces
        |= P.int


parseForward : Parser Direction
parseForward =
    succeed Forward
        |. P.keyword "forward"
        |. P.spaces
        |= P.int


parseBackward : Parser Direction
parseBackward =
    succeed Backward
        |. P.keyword "backward"
        |. P.spaces
        |= P.int
