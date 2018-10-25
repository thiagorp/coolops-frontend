module App.Api.CreateSlackProjectIntegration exposing
    ( Params
    , createSlackProjectIntegration
    )

import Api
import Http
import Json.Encode as Encode


type alias Params a =
    { a | channelName : String, channelId : String }


encode : Params a -> Encode.Value
encode params =
    Encode.object
        [ ( "channel_name", Encode.string params.channelName )
        , ( "channel_id", Encode.string params.channelId )
        ]


createSlackProjectIntegration : Api.ProtectedConfig -> String -> (Result Http.Error () -> msg) -> Params a -> Cmd msg
createSlackProjectIntegration apiConfig projectId msg params =
    Api.post apiConfig ("/projects/" ++ projectId ++ "/slack_integration") (Http.jsonBody <| encode params)
        |> Http.send msg
