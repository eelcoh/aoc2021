module Value exposing (..)


type alias Values =
    ( Int, Int )


addValues : Values -> Values -> Values
addValues ( x1, y1 ) ( x2, y2 ) =
    ( x1 + x2, y1 + y2 )


sum : List Values -> Values
sum vals =
    List.foldl addValues ( 0, 0 ) vals
