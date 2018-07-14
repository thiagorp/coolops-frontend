module App.Api.GetSlackConfig exposing (..)

import App.Api.Common exposing (..)
import Http
import Json.Decode as Decode


type alias SlackConfig =
    { enabled : Bool, clientId : String }


decoder : Decode.Decoder SlackConfig
decoder =
    Decode.map2 SlackConfig
        (Decode.field "enabled" Decode.bool)
        (Decode.field "client_id" Decode.string)


getSlackConfig : String -> Token -> (Result Http.Error SlackConfig -> msg) -> Cmd msg
getSlackConfig baseUrl token msg =
    get baseUrl token "/slack_config" (Http.expectJson decoder)
        |> Http.send msg
