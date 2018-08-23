module Main exposing (..)

import Common exposing (handleError, toProgram)
import Script exposing (Script)
import Script.Directory as Directory exposing (Directory)
import Script.File as File exposing (File)
import Script.FileSystem as FileSystem exposing (FileSystem)
import Script.Permissions as Permissions exposing (ReadWrite)


processSubdirectory : Directory ReadWrite -> String -> Script Directory.Error ()
processSubdirectory subdirectory prefix =
    let
        name =
            Directory.name subdirectory

        path =
            prefix ++ name
    in
    if name == "elm-stuff" then
        Script.do
            [ Script.printLine ("Obliterating " ++ path ++ "...")
            , Directory.obliterate subdirectory
            ]
    else if name == "node_modules" then
        Script.succeed ()
    else
        let
            updatedPrefix =
                path ++ "/"
        in
        recurseInto subdirectory updatedPrefix


processFile : File ReadWrite -> String -> Script File.Error ()
processFile file prefix =
    let
        name =
            File.name file

        path =
            prefix ++ name
    in
    if name == "elm.exe" then
        Script.do
            [ Script.printLine ("Deleting " ++ path ++ "...")
            , File.delete file
            ]
    else
        Script.succeed ()


recurseInto : Directory ReadWrite -> String -> Script Directory.Error ()
recurseInto directory prefix =
    Script.do
        [ Directory.listSubdirectories directory
            |> Script.andThen
                (Script.forEach
                    (\subdirectory -> processSubdirectory subdirectory prefix)
                )
        , Directory.listFiles directory
            |> Script.andThen
                (Script.forEach
                    (\file -> processFile file prefix)
                )
        ]


script : Script.Context -> Script Int ()
script { fileSystem } =
    let
        rootDirectory =
            fileSystem |> FileSystem.directory "C:/Git"
    in
    recurseInto rootDirectory "" |> Script.onError (handleError .message)


main : Script.Program
main =
    toProgram script
