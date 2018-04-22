port module Main exposing (..)

import Json.Encode exposing (Value)
import Script exposing (Script)
import Script.Directory as Directory
import Script.File as File
import Script.FileSystem as FileSystem
import Script.Permissions as Permissions


handleError : (x -> String) -> x -> Script Int a
handleError toMessage error =
    Script.printLine ("ERROR: " ++ toMessage error)
        |> Script.andThen (\() -> Script.fail 1)


script : Script.Context -> Script Int ()
script { fileSystem } =
    let
        sourceDirectory =
            FileSystem.directory Permissions.readOnly
                "C:/Git/opensolid/opensolid.github.io/images/geometry/icons"
                fileSystem

        destinationRoot =
            FileSystem.directory Permissions.writeOnly
                "C:/Git/ianmackenzie/ianmackenzie.github.io/elm-geometry/1.0.0"
                fileSystem
    in
    Directory.listFiles sourceDirectory
        |> Script.andThen
            (Script.forEach
                (\file ->
                    let
                        fileName =
                            File.name file
                    in
                    if String.endsWith ".svg" fileName then
                        let
                            first =
                                String.left 1 fileName

                            rest =
                                String.dropLeft 1 fileName

                            moduleName =
                                String.toUpper first ++ String.dropRight 4 rest

                            destinationFile =
                                destinationRoot
                                    |> Directory.subdirectory moduleName
                                    |> Directory.file "icon.svg"
                        in
                        File.read file
                            |> Script.andThen
                                (\contents ->
                                    File.write contents destinationFile
                                )
                    else
                        Script.do []
                )
            )
        |> Script.onError (handleError .message)


port requestPort : Value -> Cmd msg


port responsePort : (Value -> msg) -> Sub msg


main : Script.Program
main =
    Script.program script requestPort responsePort
