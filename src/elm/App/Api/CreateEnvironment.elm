module App.Api.CreateEnvironment exposing (..)

import App.Api.Common exposing (..)
import Dict exposing (Dict)
import Http
import Json.Encode as Encode


type alias Params a =
    { a | name : String, projectId : String, environmentVars : Dict String String }


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
        ]


createEnvironment : Token -> (Result Http.Error () -> msg) -> Params a -> Cmd msg
createEnvironment token msg params =
    post token ("/projects/" ++ params.projectId ++ "/environments") (Http.jsonBody <| encode params)
        |> Http.send msg
