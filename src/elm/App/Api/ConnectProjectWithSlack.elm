module App.Api.ConnectProjectWithSlack exposing (..)

import App.Api.Common exposing (..)
import Http
import Json.Encode as Encode


type alias Params a =
    { a | code : String, projectId : String }


encode : Params a -> Encode.Value
encode { code } =
    Encode.object
        [ ( "code", Encode.string code )
        ]


connectProjectWithSlack : String -> Token -> (Result Http.Error () -> msg) -> Params a -> Cmd msg
connectProjectWithSlack baseUrl token msg params =
    post
        baseUrl
        token
        ("/projects/" ++ params.projectId ++ "/slack_integration")
        (Http.jsonBody <| encode params)
        |> Http.send msg
