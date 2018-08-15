module App.Api.Common exposing (..)

import Http


type alias Token =
    String


authHeader : Token -> Http.Header
authHeader token =
    Http.header "Authorization" ("Token " ++ token)


publicGet : String -> String -> Http.Expect a -> Http.Request a
publicGet baseUrl path expect =
    Http.request
        { method = "GET"
        , headers = []
        , url = baseUrl ++ path
        , body = Http.emptyBody
        , expect = expect
        , timeout = Nothing
        , withCredentials = False
        }


get : String -> Token -> String -> Http.Expect a -> Http.Request a
get baseUrl token path expect =
    Http.request
        { method = "GET"
        , headers = [ authHeader token ]
        , url = baseUrl ++ path
        , body = Http.emptyBody
        , expect = expect
        , timeout = Nothing
        , withCredentials = False
        }


delete : String -> Token -> String -> Http.Request ()
delete baseUrl token path =
    Http.request
        { method = "DELETE"
        , headers = [ authHeader token ]
        , url = baseUrl ++ path
        , body = Http.emptyBody
        , expect = Http.expectStringResponse (\_ -> Ok ())
        , timeout = Nothing
        , withCredentials = False
        }


post : String -> Token -> String -> Http.Body -> Http.Request ()
post baseUrl token path body =
    Http.request
        { method = "POST"
        , headers = [ authHeader token ]
        , url = baseUrl ++ path
        , body = body
        , expect = Http.expectStringResponse (\_ -> Ok ())
        , timeout = Nothing
        , withCredentials = False
        }


patch : String -> Token -> String -> Http.Body -> Http.Request ()
patch baseUrl token path body =
    Http.request
        { method = "PATCH"
        , headers = [ authHeader token ]
        , url = baseUrl ++ path
        , body = body
        , expect = Http.expectStringResponse (\_ -> Ok ())
        , timeout = Nothing
        , withCredentials = False
        }
