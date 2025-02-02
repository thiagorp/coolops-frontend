module Auth.Api exposing
    ( SignupRequest
    , SignupResponse
    , login
    , signup
    )

import Api
import Http as Http
import Json.Decode as Decode
import Json.Encode as Encode



-- Signup


type alias SignupRequest a =
    { a
        | firstName : String
        , lastName : String
        , email : String
        , password : String
        , companyName : String
    }


type alias SignupResponse =
    { accessToken : String }


encodeSignup : SignupRequest a -> String
encodeSignup request =
    Encode.encode 0 <|
        Encode.object
            [ ( "first_name", Encode.string request.firstName )
            , ( "last_name", Encode.string request.lastName )
            , ( "email", Encode.string request.email )
            , ( "password", Encode.string request.password )
            , ( "company_name", Encode.string request.companyName )
            ]


decodeSignup : Decode.Decoder SignupResponse
decodeSignup =
    Decode.map SignupResponse
        (Decode.field "user_access_token" Decode.string)


signup : Api.PublicConfig -> SignupRequest a -> (Result Http.Error SignupResponse -> msg) -> Cmd msg
signup baseUrl s msg =
    Api.publicPost_ baseUrl "/signup" (Http.stringBody "application/json" <| encodeSignup s) (Http.expectJson decodeSignup)
        |> Http.send msg



-- Login


type alias LoginRequest a =
    { a
        | email : String
        , password : String
    }


type alias LoginResponse =
    { accessToken : String }


encodeLogin : LoginRequest a -> String
encodeLogin request =
    Encode.encode 0 <|
        Encode.object
            [ ( "email", Encode.string request.email )
            , ( "password", Encode.string request.password )
            ]


decodeLogin : Decode.Decoder LoginResponse
decodeLogin =
    Decode.map LoginResponse
        (Decode.field "access_token" Decode.string)


login : Api.PublicConfig -> LoginRequest a -> (Result Http.Error LoginResponse -> msg) -> Cmd msg
login apiConfig s msg =
    Api.publicPost_ apiConfig "/tokens" (Http.stringBody "application/json" <| encodeLogin s) (Http.expectJson decodeLogin)
        |> Http.send msg
