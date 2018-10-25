module App.Api.CreateSlackAccessToken exposing
    ( Params
    , createSlackAccessToken
    )

import Api
import Http
import Json.Encode as Encode


type alias Params a =
    { a | code : String }


encode : Params a -> Encode.Value
encode { code } =
    Encode.object
        [ ( "code", Encode.string code )
        ]


createSlackAccessToken : Api.ProtectedConfig -> (Result Http.Error () -> msg) -> Params a -> Cmd msg
createSlackAccessToken apiConfig msg params =
    Api.post
        apiConfig
        "/slack/access_tokens"
        (Http.jsonBody <| encode params)
        |> Http.send msg
