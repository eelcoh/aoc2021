module AdventOfCode exposing (program)

-- import Dict exposing (Dict)
-- import Direction exposing (..)
-- import Direction.Types exposing (Direction)
-- import List.Extra exposing (scanl1)

import Day1.Main
import Day2.Main
import Day3.Main
import Day4.Main
import Day5.Main
import Day6.Main
import Posix.IO as IO exposing (IO, Process)
import Posix.IO.File as File
import Posix.IO.Process as Proc


program : Process -> IO ()
program process =
    case process.argv of
        [ _, day ] ->
            IO.do
                (File.contentsOf (filename day)
                    |> IO.exitOnError identity
                )
            <|
                \content ->
                    IO.do (Proc.print (processDay day content)) <|
                        \_ ->
                            IO.return ()

        _ ->
            Proc.logErr "Usage: elm-cli <program> file\n"


processDay : String -> String -> String
processDay day content =
    case day of
        "1" ->
            Day1.Main.process content
                |> resultString day

        "2" ->
            Day2.Main.process content
                |> resultString day

        "3" ->
            Day3.Main.process content
                |> resultString day

        "4" ->
            Day4.Main.process content
                |> resultString day

        "5" ->
            Day5.Main.process content
                |> resultString day

        "6" ->
            Day6.Main.process content
                |> resultString day

        _ ->
            "not implemented"


filename : String -> String
filename day =
    "inputs/day" ++ day ++ ".txt"


resultString : String -> ( String, String ) -> String
resultString day ( part1, part2 ) =
    [ "day "
    , day
    , " - part 1 : "
    , part1
    , "\nday "
    , day
    , " - part 2 : "
    , part2
    ]
        |> String.concat
