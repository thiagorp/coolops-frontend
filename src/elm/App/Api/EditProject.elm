module App.Api.EditProject exposing (Params, editProject, encode)

import Api
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


parseResponse : String -> Http.Response String -> Result String Project
parseResponse projectId _ =
    Ok { id = projectId }


editProject : Api.ProtectedConfig -> String -> (Result Http.Error Project -> msg) -> Params a -> Cmd msg
editProject apiConfig projectId msg params =
    Api.patch_ apiConfig ("/projects/" ++ projectId) (Http.jsonBody <| encode params) (Http.expectStringResponse (parseResponse projectId))
        |> Http.send msg
