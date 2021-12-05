module Day1 exposing (program)

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
    String.lines contents
        |> List.filterMap String.toInt
        |> processValues2
        |> resultString


resultString : ( Int, Int ) -> String
resultString ( up, down ) =
    [ "there are "
    , String.fromInt up
    , " measurements that are larger than the previous measurement,"
    , "\nand "
    , String.fromInt down
    , " measurements that are smaller than the previous measurement."
    ]
        |> String.concat


processValues : List Int -> ( Int, Int )
processValues vals =
    toVerticalShifts vals
        |> countValues


processValues2 : List Int -> ( Int, Int )
processValues2 vals =
    List.Extra.groupsOfWithStep 3 1 vals
        |> List.map List.sum
        |> toVerticalShifts
        |> countValues


countValues : List Direction -> ( Int, Int )
countValues dirs =
    ( List.Extra.count isDownward dirs, List.Extra.count isUpward dirs )
