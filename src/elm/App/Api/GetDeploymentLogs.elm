module App.Api.GetDeploymentLogs exposing (..)

import App.Api.Common exposing (..)
import Http
import Json.Decode as Decode


type alias DeploymentLogs =
    { finished : Bool, logs : String }


decoder : Decode.Decoder DeploymentLogs
decoder =
    Decode.map2 DeploymentLogs
        (Decode.field "finished" Decode.bool)
        (Decode.field "logs" Decode.string)


getDeploymentLogs : String -> String -> (Result Http.Error DeploymentLogs -> msg) -> Cmd msg
getDeploymentLogs baseUrl deploymentId msg =
    publicGet baseUrl ("/deployments/" ++ deploymentId ++ "/logs") (Http.expectJson decoder)
        |> Http.send msg
