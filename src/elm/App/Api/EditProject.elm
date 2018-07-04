module App.Api.EditProject exposing (..)

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


editProject : Token -> String -> (Result Http.Error () -> msg) -> Params a -> Cmd msg
editProject token projectId msg params =
    patch token ("/projects/" ++ projectId) (Http.jsonBody <| encode params)
        |> Http.send msg
