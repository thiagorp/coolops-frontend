module App.Api.GetEnvironment exposing (..)

import App.Api.Common exposing (..)
import Dict exposing (Dict)
import Http
import Json.Decode as Decode


type alias Environment =
    { id : String
    , name : String
    , environmentVars : Dict String String
    }


decoder : Decode.Decoder Environment
decoder =
    Decode.map3 Environment
        (Decode.field "id" Decode.string)
        (Decode.field "name" Decode.string)
        (Decode.field "env_vars" (Decode.dict Decode.string))


getEnvironment : Token -> String -> (Result Http.Error Environment -> msg) -> Cmd msg
getEnvironment token id msg =
    get token ("/environments/" ++ id) (Http.expectJson decoder)
        |> Http.send msg
