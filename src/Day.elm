module Day exposing (program)

import Day1
import Dict exposing (Dict)
import Direction exposing (..)
import Direction.Types exposing (Direction)
import List.Extra exposing (scanl1)
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
            Day1.process content
                |> resultString

        _ ->
            "not implemented"


filename : String -> String
filename day =
    "inputs/day" ++ day ++ ".txt"


resultString : ( String, String ) -> String
resultString ( part1, part2 ) =
    [ "part1 : "
    , part1
    , "\npart2 : "
    , part2
    ]
        |> String.concat
