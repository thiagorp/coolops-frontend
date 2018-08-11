module App.Api.GetSlackProjectIntegration exposing (..)

import App.Api.Common exposing (..)
import Http
import Json.Decode as Decode


type alias SlackProjectIntegration =
    { enabled : Bool, clientId : String }


decoder : Decode.Decoder SlackProjectIntegration
decoder =
    Decode.map2 SlackProjectIntegration
        (Decode.field "enabled" Decode.bool)
        (Decode.field "client_id" Decode.string)


getSlackProjectIntegration : String -> Token -> String -> (Result Http.Error SlackProjectIntegration -> msg) -> Cmd msg
getSlackProjectIntegration baseUrl token projectId msg =
    get baseUrl token ("/projects/" ++ projectId ++ "/slack_integration") (Http.expectJson decoder)
        |> Http.send msg
