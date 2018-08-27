module Api exposing
    ( ApiData
    , ApiResult
    , send
    )

import Graphqelm.Http
import Graphqelm.Operation exposing (RootQuery)
import Graphqelm.SelectionSet exposing (SelectionSet)
import RemoteData as RD


type alias ApiResult a =
    Result (Graphqelm.Http.Error a) a


type alias ApiData a =
    RD.RemoteData (Graphqelm.Http.Error a) a


send : String -> String -> (ApiResult a -> msg) -> SelectionSet a RootQuery -> Cmd msg
send baseUrl token msg query =
    query
        |> Graphqelm.Http.queryRequest (baseUrl ++ "/graphql")
        |> Graphqelm.Http.withHeader "authorization" ("Token " ++ token)
        |> Graphqelm.Http.send msg
