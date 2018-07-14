module App.Api.HandleSlackOAuthCallback exposing (..)

import App.Api.Common exposing (..)
import Http
import Json.Encode as Encode


type alias Params a =
    { a | code : String }


encode : Params a -> Encode.Value
encode { code } =
    Encode.object
        [ ( "code", Encode.string code )
        ]


handleSlackOAuthCallback : String -> Token -> (Result Http.Error () -> msg) -> Params a -> Cmd msg
handleSlackOAuthCallback baseUrl token msg params =
    post baseUrl token "/slack_config" (Http.jsonBody <| encode params)
        |> Http.send msg
