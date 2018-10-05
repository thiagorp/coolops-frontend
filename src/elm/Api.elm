module Api exposing
    ( ApiData
    , ApiResult
    , BaseUrl
    , ProtectedConfig
    , PublicConfig
    , Token
    , baseUrlDecoder
    , delete
    , get
    , patch
    , patch_
    , post
    , post_
    , publicGet
    , publicPost_
    , sendGraphQL
    , tokenDecoder
    )

import Graphql.Http
import Graphql.Operation exposing (RootQuery)
import Graphql.SelectionSet exposing (SelectionSet)
import Http
import Json.Decode as Decode
import RemoteData as RD


type alias ApiResult a =
    Result (Graphql.Http.Error a) a


type alias ApiData a =
    RD.RemoteData (Graphql.Http.Error a) a


type Token
    = Token String


type BaseUrl
    = BaseUrl String


type alias PublicConfig =
    BaseUrl


type alias ProtectedConfig =
    { token : Token
    , baseUrl : BaseUrl
    }


type alias FlagsResult =
    { token : Maybe Token
    , baseUrl : BaseUrl
    }


tokenDecoder : Decode.Decoder Token
tokenDecoder =
    Decode.map Token Decode.string


baseUrlDecoder : Decode.Decoder BaseUrl
baseUrlDecoder =
    Decode.map BaseUrl Decode.string


readFlags : Decode.Value -> Result Decode.Error FlagsResult
readFlags =
    Decode.map2 FlagsResult
        (Decode.field "token" (Decode.nullable (Decode.map Token Decode.string)))
        (Decode.field "baseUrl" (Decode.map BaseUrl Decode.string))
        |> Decode.decodeValue


readBaseUrl : BaseUrl -> String
readBaseUrl (BaseUrl url) =
    url


readToken : Token -> String
readToken (Token token) =
    token


sendGraphQL : ProtectedConfig -> (ApiResult a -> msg) -> SelectionSet a RootQuery -> Cmd msg
sendGraphQL { token, baseUrl } msg query =
    query
        |> Graphql.Http.queryRequest (readBaseUrl baseUrl ++ "/graphql")
        |> Graphql.Http.withHeader "authorization" ("Token " ++ readToken token)
        |> Graphql.Http.send msg


authHeader : Token -> Http.Header
authHeader token =
    Http.header "Authorization" ("Token " ++ readToken token)


publicGet : PublicConfig -> String -> Http.Expect a -> Http.Request a
publicGet baseUrl path expect =
    Http.request
        { method = "GET"
        , headers = []
        , url = readBaseUrl baseUrl ++ path
        , body = Http.emptyBody
        , expect = expect
        , timeout = Nothing
        , withCredentials = False
        }


publicPost_ : PublicConfig -> String -> Http.Body -> Http.Expect a -> Http.Request a
publicPost_ baseUrl path body expect =
    Http.request
        { method = "POST"
        , headers = []
        , url = readBaseUrl baseUrl ++ path
        , body = body
        , expect = expect
        , timeout = Nothing
        , withCredentials = False
        }


get : ProtectedConfig -> String -> Http.Expect a -> Http.Request a
get { token, baseUrl } path expect =
    Http.request
        { method = "GET"
        , headers = [ authHeader token ]
        , url = readBaseUrl baseUrl ++ path
        , body = Http.emptyBody
        , expect = expect
        , timeout = Nothing
        , withCredentials = False
        }


delete : ProtectedConfig -> String -> Http.Request ()
delete { token, baseUrl } path =
    Http.request
        { method = "DELETE"
        , headers = [ authHeader token ]
        , url = readBaseUrl baseUrl ++ path
        , body = Http.emptyBody
        , expect = Http.expectStringResponse (\_ -> Ok ())
        , timeout = Nothing
        , withCredentials = False
        }


post_ : ProtectedConfig -> String -> Http.Body -> Http.Expect a -> Http.Request a
post_ { baseUrl, token } path body expect =
    Http.request
        { method = "POST"
        , headers = [ authHeader token ]
        , url = readBaseUrl baseUrl ++ path
        , body = body
        , expect = expect
        , timeout = Nothing
        , withCredentials = False
        }


post : ProtectedConfig -> String -> Http.Body -> Http.Request ()
post config path body =
    post_
        config
        path
        body
        (Http.expectStringResponse (\_ -> Ok ()))


patch_ : ProtectedConfig -> String -> Http.Body -> Http.Expect a -> Http.Request a
patch_ { baseUrl, token } path body expect =
    Http.request
        { method = "PATCH"
        , headers = [ authHeader token ]
        , url = readBaseUrl baseUrl ++ path
        , body = body
        , expect = expect
        , timeout = Nothing
        , withCredentials = False
        }


patch : ProtectedConfig -> String -> Http.Body -> Http.Request ()
patch config path body =
    patch_
        config
        path
        body
        (Http.expectStringResponse (\_ -> Ok ()))
