module Day4 exposing (program)

import Bingo exposing (..)
import Dict exposing (Dict)
import List.Extra exposing (dropWhile, scanl1)
import List.Split exposing (split)
import Posix.IO as IO exposing (IO, Process)
import Posix.IO.File as File
import Posix.IO.Process as Proc


program : Process -> IO ()
program process =
    case process.argv of
        [ _, filename ] ->
            IO.do
                (File.contentsOf filename
                    |> IO.exitOnError identity
                )
            <|
                \content ->
                    IO.do (Proc.print (processFileContents content)) <|
                        \_ ->
                            IO.return ()

        _ ->
            Proc.logErr "Usage: elm-cli <program> file\n"


processFileContents : String -> String
processFileContents contents =
    let
        cards =
            String.lines contents
                |> split (\s -> s == "")
                |> parseCards
                |> processCards values

        printCard_ ( card, cardV, v ) =
            String.join "\n" [ printCard card, String.fromInt cardV, String.fromInt v, String.fromInt (v * cardV) ]
    in
    List.map printCard_ cards
        |> String.join "\n\n"


processCards : List Int -> List Card -> List ( Card, Int, Int )
processCards vals cards =
    case vals of
        v :: rest ->
            let
                processedCards =
                    List.map (newValue v) cards

                foundBingo =
                    List.filter Tuple.second processedCards
            in
            if List.isEmpty foundBingo then
                processCards rest (List.map Tuple.first processedCards)

            else
                List.map Tuple.first foundBingo
                    |> List.map (\c -> ( c, cardValue c, v ))

        _ ->
            []



-- |> List.filterMap parseNoise
-- |> noiseValues
-- |> resultString


resultString : ( Int, Int ) -> String
resultString ( zeroes, ones ) =
    let
        zeroesTxt =
            "there are " ++ String.fromInt zeroes ++ " strings with a majority of zeroes"

        onesTxt =
            "\nand there are " ++ String.fromInt ones ++ " strings with a majority of ones"
    in
    [ zeroesTxt
    , onesTxt
    ]
        |> String.concat


values =
    [ 10
    , 80
    , 6
    , 69
    , 22
    , 99
    , 63
    , 92
    , 30
    , 67
    , 28
    , 93
    , 0
    , 50
    , 65
    , 87
    , 38
    , 7
    , 91
    , 60
    , 57
    , 40
    , 84
    , 51
    , 27
    , 12
    , 44
    , 88
    , 64
    , 35
    , 39
    , 74
    , 61
    , 55
    , 31
    , 48
    , 81
    , 89
    , 62
    , 37
    , 94
    , 43
    , 29
    , 14
    , 95
    , 8
    , 78
    , 49
    , 90
    , 97
    , 66
    , 70
    , 25
    , 68
    , 75
    , 45
    , 42
    , 23
    , 9
    , 96
    , 56
    , 72
    , 59
    , 32
    , 85
    , 3
    , 71
    , 79
    , 18
    , 24
    , 33
    , 19
    , 15
    , 20
    , 82
    , 26
    , 21
    , 13
    , 4
    , 98
    , 83
    , 34
    , 86
    , 5
    , 2
    , 73
    , 17
    , 54
    , 1
    , 77
    , 52
    , 58
    , 76
    , 36
    , 16
    , 46
    , 41
    , 47
    , 11
    , 53
    ]
