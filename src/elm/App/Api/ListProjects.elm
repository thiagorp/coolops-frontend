module App.Api.ListProjects exposing (..)

import App.Api.Common exposing (..)
import Http
import Json.Decode as Decode


type alias Project =
    { id : String, name : String, deploymentImage : String }


decoder : Decode.Decoder Project
decoder =
    Decode.map3 Project
        (Decode.field "id" Decode.string)
        (Decode.field "name" Decode.string)
        (Decode.field "deployment_image" Decode.string)


listProjects : Token -> (Result Http.Error (List Project) -> msg) -> Cmd msg
listProjects token msg =
    get token "/projects" (Http.expectJson (Decode.list decoder))
        |> Http.send msg
