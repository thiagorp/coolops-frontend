module App.Api.Common exposing (..)

import Http


type alias Token =
    String


authHeader : Token -> Http.Header
authHeader token =
    Http.header "Authorization" ("Token " ++ token)


baseUrl : String
baseUrl =
    "http://localhost:3001"


get : Token -> String -> Http.Expect a -> Http.Request a
get token path expect =
    Http.request
        { method = "GET"
        , headers = [ authHeader token ]
        , url = baseUrl ++ path
        , body = Http.emptyBody
        , expect = expect
        , timeout = Nothing
        , withCredentials = False
        }


delete : Token -> String -> Http.Request ()
delete token path =
    Http.request
        { method = "DELETE"
        , headers = [ authHeader token ]
        , url = baseUrl ++ path
        , body = Http.emptyBody
        , expect = Http.expectStringResponse (\_ -> Ok ())
        , timeout = Nothing
        , withCredentials = False
        }


post : Token -> String -> Http.Body -> Http.Request ()
post token path body =
    Http.request
        { method = "POST"
        , headers = [ authHeader token ]
        , url = baseUrl ++ path
        , body = body
        , expect = Http.expectStringResponse (\_ -> Ok ())
        , timeout = Nothing
        , withCredentials = False
        }
