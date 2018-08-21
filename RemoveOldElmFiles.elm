module Main exposing (..)

import Common exposing (handleError, toProgram)
import Script exposing (Script)
import Script.Directory as Directory exposing (Directory)
import Script.File as File exposing (File)
import Script.FileSystem as FileSystem exposing (FileSystem)
import Script.Permissions as Permissions exposing (ReadWrite)


findRecursively : String -> Directory ReadWrite -> Script Directory.Error ()
findRecursively prefix directory =
    Script.do
        [ Directory.listSubdirectories directory
            |> Script.andThen
                (Script.forEach
                    (\subdirectory ->
                        let
                            subdirectoryName =
                                Directory.name subdirectory
                        in
                        if subdirectoryName == "elm-stuff" then
                            Script.do
                                [ Script.printLine <|
                                    "Obliterating "
                                        ++ prefix
                                        ++ subdirectoryName
                                        ++ "..."
                                , Directory.obliterate subdirectory
                                ]
                        else if subdirectoryName == "node_modules" then
                            Script.do []
                        else
                            let
                                updatedPrefix =
                                    prefix ++ subdirectoryName ++ "/"
                            in
                            findRecursively updatedPrefix subdirectory
                    )
                )
        , Directory.listFiles directory
            |> Script.andThen
                (Script.forEach
                    (\file ->
                        let
                            fileName =
                                File.name file
                        in
                        if fileName == "elm.exe" then
                            Script.do
                                [ Script.printLine
                                    ("Deleting " ++ prefix ++ fileName ++ "...")
                                , File.delete file
                                ]
                        else
                            Script.do []
                    )
                )
        ]


script : Script.Context -> Script Int ()
script { fileSystem } =
    let
        rootDirectory =
            fileSystem |> FileSystem.directory "C:/Git"
    in
    findRecursively "" rootDirectory |> Script.onError (handleError .message)


main : Script.Program
main =
    toProgram script
