port module Ports exposing (..)


port login : String -> Cmd msg


port onSessionChange : (Maybe String -> msg) -> Sub msg
