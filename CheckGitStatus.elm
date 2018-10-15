module Main exposing (main)

import Common exposing (handleError, toProgram)
import Script exposing (Script)
import Script.Directory as Directory exposing (Directory)
import Script.File as File exposing (File)
import Script.FileSystem as FileSystem exposing (FileSystem)
import Script.Permissions as Permissions exposing (ReadOnly)
import Script.Shell as Shell exposing (Shell)


main : Script.Program
main =
    toProgram script


script : Script.Context -> Script Int ()
script { shell, fileSystem } =
    let
        rootDirectory =
            fileSystem |> FileSystem.directory "C:/Git"
    in
    recurseInto rootDirectory "" shell |> Script.onError (handleError .message)


recurseInto : Directory ReadOnly -> String -> Shell -> Script Directory.Error ()
recurseInto directory prefix shell =
    let
        dotGitDirectory =
            directory |> Directory.subdirectory ".git"
    in
    Directory.checkExistence dotGitDirectory
        |> Script.andThen
            (\existence ->
                case existence of
                    Directory.Exists ->
                        Script.printLine "TODO run 'git status'"

                    Directory.DoesNotExist ->
                        Script.printLine "TODO recurse into child directories"

                    Directory.IsNotADirectory ->
                        Script.printLine
                            "Error - .git exists but is not a directory"
            )
