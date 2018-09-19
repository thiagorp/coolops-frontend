module Api exposing
    ( ApiData
    , ApiResult
    , send
    )

import Graphql.Http
import Graphql.Operation exposing (RootQuery)
import Graphql.SelectionSet exposing (SelectionSet)
import RemoteData as RD


type alias ApiResult a =
    Result (Graphql.Http.Error a) a


type alias ApiData a =
    RD.RemoteData (Graphql.Http.Error a) a


send : String -> String -> (ApiResult a -> msg) -> SelectionSet a RootQuery -> Cmd msg
send baseUrl token msg query =
    query
        |> Graphql.Http.queryRequest (baseUrl ++ "/graphql")
        |> Graphql.Http.withHeader "authorization" ("Token " ++ token)
        |> Graphql.Http.send msg
