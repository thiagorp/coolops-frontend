module App.Api.UpdateEnvironment exposing (Params, encode, encodeEnvVars, updateEnvironment)

import Api
import Dict exposing (Dict)
import Http
import Json.Encode as Encode


type alias Params a =
    { a | name : String, slug : String, environmentVars : Dict String String }


encodeEnvVars : Dict String String -> Encode.Value
encodeEnvVars dict =
    Dict.toList dict
        |> List.map (\( k, v ) -> ( k, Encode.string v ))
        |> Encode.object


encode : Params a -> Encode.Value
encode params =
    Encode.object
        [ ( "name", Encode.string params.name )
        , ( "slug", Encode.string params.slug )
        , ( "env_vars", encodeEnvVars params.environmentVars )
        ]


updateEnvironment : Api.ProtectedConfig -> String -> (Result Http.Error () -> msg) -> Params a -> Cmd msg
updateEnvironment apiConfig environmentId msg params =
    Api.patch apiConfig ("/environments/" ++ environmentId) (Http.jsonBody <| encode params)
        |> Http.send msg
