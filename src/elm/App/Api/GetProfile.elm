module App.Api.GetProfile exposing (..)

import App.Api.Common exposing (..)
import Http
import Json.Decode as Decode


type alias User =
    { id : String, first_name : String, last_name : String, email : String }


type alias Company =
    { id : String, name : String }


type alias Profile =
    { user : User
    , company : Company
    }


decodeUser : Decode.Decoder User
decodeUser =
    Decode.map4 User
        (Decode.field "id" Decode.string)
        (Decode.field "first_name" Decode.string)
        (Decode.field "last_name" Decode.string)
        (Decode.field "email" Decode.string)


decodeCompany : Decode.Decoder Company
decodeCompany =
    Decode.map2 Company
        (Decode.field "id" Decode.string)
        (Decode.field "name" Decode.string)


decoder : Decode.Decoder Profile
decoder =
    Decode.map2 Profile
        (Decode.field "user" decodeUser)
        (Decode.field "company" decodeCompany)


getProfile : Token -> (Result Http.Error Profile -> msg) -> Cmd msg
getProfile token msg =
    get token "/me" (Http.expectJson decoder)
        |> Http.send msg
