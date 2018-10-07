module App.Api.CreateEnvironment exposing (Params, createEnvironment, encode, encodeEnvVars)

import Api
import Dict exposing (Dict)
import Http
import Json.Encode as Encode


type alias Params a =
    { a | name : String, projectId : String, slug : String, environmentVars : Dict String String }


encodeEnvVars : Dict String String -> Encode.Value
encodeEnvVars dict =
    Dict.toList dict
        |> List.map (\( k, v ) -> ( k, Encode.string v ))
        |> Encode.object


encode : Params a -> Encode.Value
encode params =
    Encode.object
        [ ( "name", Encode.string params.name )
        , ( "env_vars", encodeEnvVars params.environmentVars )
        , ( "slug", Encode.string params.slug )
        ]


createEnvironment : Api.ProtectedConfig -> (Result Http.Error () -> msg) -> Params a -> Cmd msg
createEnvironment apiConfig msg params =
    Api.post apiConfig ("/projects/" ++ params.projectId ++ "/environments") (Http.jsonBody <| encode params)
        |> Http.send msg
