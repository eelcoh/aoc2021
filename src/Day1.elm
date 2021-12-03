module Day1 exposing (program)

import Dict exposing (Dict)
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


type Direction
    = Increase
    | Decrease
    | Unchanged


processFileContents : String -> String
processFileContents contents =
    String.lines contents
        |> List.filterMap String.toInt
        |> processValues
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
    toDirections vals
        |> countValues


countValues : List Direction -> ( Int, Int )
countValues dirs =
    ( List.Extra.count increased dirs, List.Extra.count decreased dirs )


increased : Direction -> Bool
increased dir =
    dir == Increase


decreased : Direction -> Bool
decreased dir =
    dir == Decrease


toDirections : List Int -> List Direction
toDirections vals =
    case vals of
        h :: t ->
            List.map2 toDirection vals t

        _ ->
            []


toDirection : Int -> Int -> Direction
toDirection a b =
    if b < a then
        Decrease

    else if b > a then
        Increase

    else
        Unchanged
