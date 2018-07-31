port module Main exposing (..)

import Dict exposing (Dict)
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


readFile : File (Read p) -> Script Int (Dict String String)
readFile file =
    File.read file
        |> Script.onError (handleError .message)
        |> Script.andThen
            (\contents ->
                case Decode.decodeString (Decode.list moduleDecoder) contents of
                    Ok modules ->
                        Script.succeed modules

                    Err decodeError ->
                        handleError identity (Debug.toString decodeError)
            )
        |> Script.map
            (\modules ->
                modules
                    |> List.map members
                    |> List.concat
                    |> Dict.fromList
            )


stripModuleName : String -> String
stripModuleName name =
    if String.startsWith "OpenSolid." name then
        String.dropLeft 10 name
    else
        name


typeRegex : Regex
typeRegex =
    Regex.fromString "(\\w+\\.)*(\\w+)" |> Maybe.withDefault Regex.never


commaRegex : Regex
commaRegex =
    Regex.fromString "\\s+," |> Maybe.withDefault Regex.never


stripTypeNames : String -> String
stripTypeNames type_ =
    type_
        |> Regex.replace
            typeRegex
            (\match ->
                match.submatches
                    |> List.filterMap identity
                    |> List.reverse
                    |> List.head
                    |> Maybe.withDefault ""
            )
        |> Regex.replace
            commaRegex
            (always ",")


members : Module -> List ( String, String )
members mod =
    let
        moduleName =
            stripModuleName mod.name
    in
    if
        (moduleName == "Geometry.Decode")
            || (moduleName == "Geometry.Encode")
            || (moduleName == "Interval")
            || (moduleName == "Scalar")
    then
        []
    else
        mod.members
            |> List.map
                (\{ name, type_ } ->
                    ( moduleName ++ "." ++ name
                    , stripTypeNames type_
                    )
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
                "C:/Git/ianmackenzie/elm-geometry-0.18/docs.json"
                fileSystem

        oldOutputFile =
            FileSystem.file Permissions.writeOnly
                "C:/Git/ianmackenzie/elm-geometry-0.18/old.txt"
                fileSystem

        newOutputFile =
            FileSystem.file Permissions.writeOnly
                "C:/Git/ianmackenzie/elm-geometry-0.18/new.txt"
                fileSystem
    in
    Script.map2 Tuple.pair (readFile oldInputFile) (readFile newInputFile)
        |> Script.andThen
            (\( oldMembers, newMembers ) ->
                let
                    join name type_ =
                        name ++ ": " ++ type_

                    ( oldAccumulated, newAccumulated ) =
                        Dict.merge
                            (\oldName oldType ( old, new ) ->
                                ( join oldName oldType :: old
                                , "" :: new
                                )
                            )
                            (\name oldType newType ( old, new ) ->
                                if oldType == newType then
                                    ( old, new )
                                else
                                    ( join name oldType :: old
                                    , join name newType :: new
                                    )
                            )
                            (\newName newType ( old, new ) ->
                                ( "" :: old
                                , join newName newType :: new
                                )
                            )
                            oldMembers
                            newMembers
                            ( [], [] )

                    oldLines =
                        List.reverse oldAccumulated

                    newLines =
                        List.reverse newAccumulated
                in
                Script.do
                    [ File.writeTo oldOutputFile (String.join "\n" oldLines)
                    , File.writeTo newOutputFile (String.join "\n" newLines)
                    ]
                    |> Script.onError (handleError .message)
            )


port requestPort : Value -> Cmd msg


port responsePort : (Value -> msg) -> Sub msg


main : Script.Program
main =
    Script.program script requestPort responsePort
