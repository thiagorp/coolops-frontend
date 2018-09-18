module App.Api.CreateProject exposing (Params, Project, createProject, decode, encode)

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


createProject : String -> Token -> (Result Http.Error Project -> msg) -> Params a -> Cmd msg
createProject baseUrl token msg params =
    post_ baseUrl token "/projects" (Http.jsonBody <| encode params) (Http.expectJson decode)
        |> Http.send msg
