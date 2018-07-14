module App.Api.CreateProject exposing (..)

import App.Api.Common exposing (..)
import Http
import Json.Encode as Encode


type alias Params a =
    { a | name : String, deploymentImage : String }


encode : Params a -> Encode.Value
encode params =
    Encode.object
        [ ( "name", Encode.string params.name )
        , ( "deployment_image", Encode.string params.deploymentImage )
        ]


createProject : String -> Token -> (Result Http.Error () -> msg) -> Params a -> Cmd msg
createProject baseUrl token msg params =
    post baseUrl token "/projects" (Http.jsonBody <| encode params)
        |> Http.send msg
