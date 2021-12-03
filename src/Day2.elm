module Day2 exposing (program)

import Dict exposing (Dict)
import Direction exposing (..)
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
        |> List.filterMap parseDirection
        |> calculateChange
        |> resultString


resultString : ( Int, Int ) -> String
resultString ( vertical, horizontal ) =
    let
        vertText =
            if vertical > 0 then
                "we moved " ++ String.fromInt vertical ++ " down"

            else if vertical < 0 then
                "we moved " ++ String.fromInt vertical ++ " up"

            else
                "we did not move vertically"

        horiText =
            if horizontal > 0 then
                "we moved " ++ String.fromInt horizontal ++ " forward"

            else if horizontal < 0 then
                "we moved " ++ String.fromInt horizontal ++ " backward"

            else
                "we did not move horizontally"
    in
    [ vertText
    , "\nand "
    , horiText
    ]
        |> String.concat
