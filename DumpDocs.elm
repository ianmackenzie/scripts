port module Main exposing (..)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode exposing (Value)
import Regex exposing (Regex)
import Script exposing (Script)
import Script.File as File exposing (File)
import Script.FileSystem as FileSystem
import Script.Permissions as Permissions exposing (Read, Write)


handleError : (x -> String) -> x -> Script Int a
handleError toMessage error =
    Script.printLine ("ERROR: " ++ toMessage error)
        |> Script.andThen (\() -> Script.fail 1)


type alias Member =
    { name : String
    , type_ : String
    }


type alias Module =
    { name : String
    , members : List Member
    }


memberDecoder : Decoder Member
memberDecoder =
    Decode.map2 Member
        (Decode.field "name" Decode.string)
        (Decode.field "type" Decode.string)


moduleDecoder : Decoder Module
moduleDecoder =
    Decode.map2 Module
        (Decode.field "name" Decode.string)
        (Decode.field "values" (Decode.list memberDecoder))


readFile : File (Read p) -> Script Int (List Module)
readFile file =
    File.read file
        |> Script.onError (handleError .message)
        |> Script.andThen
            (\contents ->
                case Decode.decodeString (Decode.list moduleDecoder) contents of
                    Ok modules ->
                        Script.succeed modules

                    Err message ->
                        handleError identity message
            )


stripModuleName : String -> String
stripModuleName name =
    if String.startsWith "OpenSolid." name then
        String.dropLeft 10 name
    else
        name


typeRegex : Regex
typeRegex =
    Regex.regex "(\\w+\\.)*(\\w+)"


commaRegex : Regex
commaRegex =
    Regex.regex "\\s+,"


stripTypeNames : String -> String
stripTypeNames type_ =
    type_
        |> Regex.replace
            Regex.All
            typeRegex
            (\match ->
                match.submatches
                    |> List.filterMap identity
                    |> List.reverse
                    |> List.head
                    |> Maybe.withDefault ""
            )
        |> Regex.replace
            Regex.All
            commaRegex
            (always ",")


members : Module -> List String
members mod =
    let
        moduleName =
            stripModuleName mod.name
    in
    mod.members
        |> List.map
            (\{ name, type_ } ->
                moduleName ++ "." ++ name ++ ": " ++ stripTypeNames type_
            )


dump : File (Read p) -> File (Write p) -> Script Int ()
dump inputFile outputFile =
    readFile inputFile
        |> Script.andThen
            (\modules ->
                let
                    allMembers =
                        modules
                            |> List.map members
                            |> List.concat
                            |> List.sort
                in
                File.writeTo outputFile (String.join "\n" allMembers)
                    |> Script.onError (handleError .message)
            )


script : Script.Context -> Script Int ()
script { fileSystem } =
    let
        oldInputFile =
            FileSystem.file Permissions.readOnly
                "C:/Users/ianem/Git/opensolid/geometry/docs.json"
                fileSystem

        newInputFile =
            FileSystem.file Permissions.readOnly
                "C:/Git/ianmackenzie/elm-geometry/docs.json"
                fileSystem

        oldOutputFile =
            FileSystem.file Permissions.writeOnly
                "C:/Git/ianmackenzie/elm-geometry/old.txt"
                fileSystem

        newOutputFile =
            FileSystem.file Permissions.writeOnly
                "C:/Git/ianmackenzie/elm-geometry/new.txt"
                fileSystem
    in
    Script.do
        [ dump oldInputFile oldOutputFile
        , dump newInputFile newOutputFile
        ]


port requestPort : Value -> Cmd msg


port responsePort : (Value -> msg) -> Sub msg


main : Script.Program
main =
    Script.program script requestPort responsePort
