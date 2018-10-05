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


decode : Decode.Decoder Project
decode =
    Decode.map Project (Decode.field "id" Decode.string)


editProject : Api.ProtectedConfig -> String -> (Result Http.Error Project -> msg) -> Params a -> Cmd msg
editProject apiConfig projectId msg params =
    Api.patch_ apiConfig ("/projects/" ++ projectId) (Http.jsonBody <| encode params) (Http.expectJson decode)
        |> Http.send msg
