module Day3 exposing (program)

import List.Extra exposing (scanl1)
import Noise exposing (..)
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
        gamma =
            String.lines contents
                |> List.filterMap parseNoise
                |> toGamma

        epsilon =
            toEpsilon gamma

        gammaValue =
            bitsToValue gamma

        epsilonValue =
            bitsToValue epsilon

        rs =
            [ "gamma : "
                ++ bitsToString gamma
                ++ " - "
                ++ String.fromInt gammaValue
            , "epsilon : "
                ++ bitsToString epsilon
                ++ " - "
                ++ String.fromInt epsilonValue
            , "total : "
                ++ String.fromInt (epsilonValue * gammaValue)
            ]
    in
    String.join "\n" rs


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
