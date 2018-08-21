port module Common exposing (handleError, toProgram)

import Json.Encode exposing (Value)
import Script exposing (Script)


handleError : (x -> String) -> x -> Script Int a
handleError toMessage error =
    Script.printLine ("- SCRIPT ERROR - " ++ toMessage error)
        |> Script.andThen (\() -> Script.fail 1)


port requestPort : Value -> Cmd msg


port responsePort : (Value -> msg) -> Sub msg


toProgram : (Script.Context -> Script Int ()) -> Script.Program
toProgram script =
    Script.program script requestPort responsePort
