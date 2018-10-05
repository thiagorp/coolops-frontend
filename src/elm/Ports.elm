port module Ports exposing (login, logout, onSessionChange)

import Json.Decode as Decode


port login : String -> Cmd msg


port logout : () -> Cmd msg


port onSessionChange : (Decode.Value -> msg) -> Sub msg
