module App.Api.ConnectProjectWithSlack exposing (Params, connectProjectWithSlack, encode)

import Api
import Http
import Json.Encode as Encode


type alias Params a =
    { a | code : String, projectId : String }


encode : Params a -> Encode.Value
encode { code } =
    Encode.object
        [ ( "code", Encode.string code )
        ]


connectProjectWithSlack : Api.ProtectedConfig -> (Result Http.Error () -> msg) -> Params a -> Cmd msg
connectProjectWithSlack apiConfig msg params =
    Api.post
        apiConfig
        ("/projects/" ++ params.projectId ++ "/slack_integration")
        (Http.jsonBody <| encode params)
        |> Http.send msg
