module Bingo exposing (..)

import Parser as P exposing ((|.), (|=), Parser, succeed)


type Range
    = Range Cell Cell Cell Cell Cell


type alias Cell =
    ( Int, Bool )


type Card
    = Card Range Range Range Range Range


range c1 c2 c3 c4 c5 =
    Range
        c1
        c2
        c3
        c4
        c5


cell i =
    ( i, False )


ranges : Card -> List Range
ranges (Card r1 r2 r3 r4 r5) =
    [ r1, r2, r3, r4, r5 ]


cells : Range -> List Cell
cells (Range c1 c2 c3 c4 c5) =
    [ c1, c2, c3, c4, c5 ]


transpose : Card -> Card
transpose (Card (Range c11 c12 c13 c14 c15) (Range c21 c22 c23 c24 c25) (Range c31 c32 c33 c34 c35) (Range c41 c42 c43 c44 c45) (Range c51 c52 c53 c54 c55)) =
    Card
        (Range c11 c21 c31 c41 c51)
        (Range c12 c22 c32 c42 c52)
        (Range c13 c23 c33 c43 c53)
        (Range c14 c24 c34 c44 c54)
        (Range c15 c25 c35 c45 c55)


newValue : Int -> Card -> ( Card, Bool )
newValue v card =
    let
        newCard =
            cardNewValue v card
    in
    ( newCard, bingo newCard )


bingo : Card -> Bool
bingo card =
    bingoCard card || bingoCard (transpose card)


bingoCard : Card -> Bool
bingoCard card =
    List.any bingoRange <| ranges card


bingoRange : Range -> Bool
bingoRange r =
    List.all Tuple.second <| cells r


cardSum : Card -> Int
cardSum card =
    ranges card
        |> List.map rangeSum
        |> List.sum


rangeSum r =
    cells r
        |> List.filter Tuple.second
        |> List.map (\c -> Tuple.first c)
        |> List.sum


cardNewValue : Int -> Card -> Card
cardNewValue v (Card r1 r2 r3 r4 r5) =
    Card
        (rangeNewValue v r1)
        (rangeNewValue v r2)
        (rangeNewValue v r3)
        (rangeNewValue v r4)
        (rangeNewValue v r5)


rangeNewValue : Int -> Range -> Range
rangeNewValue v (Range c1 c2 c3 c4 c5) =
    Range
        (cellNewValue v c1)
        (cellNewValue v c2)
        (cellNewValue v c3)
        (cellNewValue v c4)
        (cellNewValue v c5)


cellNewValue : Int -> Cell -> Cell
cellNewValue v ( cv, taken ) =
    if v == cv then
        ( cv, True )

    else
        ( cv, taken )



-- parsers


parseCards : List (List String) -> List Card
parseCards cardStringLists =
    List.map (String.join " | ") cardStringLists
        |> List.filterMap parseCard


parseCard : String -> Maybe Card
parseCard cardString =
    P.run parseCard_ cardString
        |> Result.toMaybe


parseCard_ : Parser Card
parseCard_ =
    P.succeed Card
        |= parseRange
        |. P.spaces
        |. P.symbol "|"
        |. P.spaces
        |= parseRange
        |. P.spaces
        |. P.symbol "|"
        |. P.spaces
        |= parseRange
        |. P.spaces
        |. P.symbol "|"
        |. P.spaces
        |= parseRange
        |. P.spaces
        |. P.symbol "|"
        |. P.spaces
        |= parseRange


parseRange : Parser Range
parseRange =
    P.succeed range
        |= parseCell
        |. P.spaces
        |= parseCell
        |. P.spaces
        |= parseCell
        |. P.spaces
        |= parseCell
        |. P.spaces
        |= parseCell


parseCell : Parser Cell
parseCell =
    succeed cell
        |= P.int



-- print


printCard c =
    let
        rangeStrings =
            ranges c
                |> List.map printRange
    in
    String.join "\n" ("========= Card =========" :: rangeStrings)


printRange r =
    cells r
        |> List.map printCell
        |> String.join " "


printCell ( v, t ) =
    if not t then
        " " ++ printInt v ++ " "

    else
        "[" ++ printInt v ++ "]"


printInt i =
    if i < 10 then
        " " ++ String.fromInt i

    else
        String.fromInt i



-- values


cardValue c =
    List.map rangeValue (ranges c)
        |> List.sum


rangeValue r =
    List.map cellValue (cells r)
        |> List.sum


cellValue ( v, t ) =
    if t then
        0

    else
        v
