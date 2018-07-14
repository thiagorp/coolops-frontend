module App.Api.UpdateEnvironment exposing (..)

import App.Api.Common exposing (..)
import Dict exposing (Dict)
import Http
import Json.Encode as Encode


type alias Params a =
    { a | name : String, environmentVars : Dict String String }


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


updateEnvironment : String -> Token -> String -> (Result Http.Error () -> msg) -> Params a -> Cmd msg
updateEnvironment baseUrl token environmentId msg params =
    patch baseUrl token ("/environments/" ++ environmentId) (Http.jsonBody <| encode params)
        |> Http.send msg
