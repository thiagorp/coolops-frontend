module App.Api.GetProject exposing (..)

import App.Api.Common exposing (..)
import Http
import Json.Decode as Decode


type alias Project =
    { id : String
    , name : String
    , deploymentImage : String
    , accessToken : String
    }


decoder : Decode.Decoder Project
decoder =
    Decode.map4 Project
        (Decode.field "id" Decode.string)
        (Decode.field "name" Decode.string)
        (Decode.field "deployment_image" Decode.string)
        (Decode.field "access_token" Decode.string)


getProject : Token -> String -> (Result Http.Error Project -> msg) -> Cmd msg
getProject token id msg =
    get token ("/projects/" ++ id) (Http.expectJson decoder)
        |> Http.send msg
