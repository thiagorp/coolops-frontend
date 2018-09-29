module App.Api.EditProject exposing (Params, editProject, encode)

import App.Api.Common exposing (..)
import Http
import Json.Decode as Decode
import Json.Encode as Encode


type alias Params a =
    { a | name : String, slug : String, deploymentImage : String }


encode : Params a -> Encode.Value
encode params =
    Encode.object
        [ ( "name", Encode.string params.name )
        , ( "slug", Encode.string params.slug )
        , ( "deployment_image", Encode.string params.deploymentImage )
        ]


type alias Project =
    { id : String }


decode : Decode.Decoder Project
decode =
    Decode.map Project (Decode.field "id" Decode.string)


editProject : String -> Token -> String -> (Result Http.Error Project -> msg) -> Params a -> Cmd msg
editProject baseUrl token projectId msg params =
    patch_ baseUrl token ("/projects/" ++ projectId) (Http.jsonBody <| encode params) (Http.expectJson decode)
        |> Http.send msg
