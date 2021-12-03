module Direction.Types exposing (..)


type Direction
    = Downward Int
    | Upward Int
    | Forward Int
    | Backward Int
    | Unchanged


type alias Values =
    ( Int, Int )
