module Public.Pages.DeploymentLogs.Data exposing
    ( DeploymentLogs
    , getDeploymentLogs
    )

import Api
import Http
import Json.Decode as Decode


type alias DeploymentLogs =
    { finished : Bool, logs : String }


decoder : Decode.Decoder DeploymentLogs
decoder =
    Decode.map2 DeploymentLogs
        (Decode.field "finished" Decode.bool)
        (Decode.field "logs" Decode.string)


getDeploymentLogs : Api.BaseUrl -> String -> (Result Http.Error DeploymentLogs -> msg) -> Cmd msg
getDeploymentLogs baseUrl deploymentId msg =
    Api.publicGet baseUrl ("/deployments/" ++ deploymentId ++ "/logs") (Http.expectJson decoder)
        |> Http.send msg
