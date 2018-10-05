module App.Fragments.Topbar.Data exposing
    ( Company
    , User
    , getProfile
    )

import Api
import Api.Object as Api
import Api.Object.Company as CompanyApi
import Api.Object.User as UserApi
import Api.Query as Query
import Graphql.Operation exposing (RootQuery)
import Graphql.SelectionSet exposing (SelectionSet, with)


type alias Company =
    { name : String }


type alias User =
    { firstName : String
    , lastName : String
    , company : Company
    }


company : SelectionSet Company Api.Company
company =
    CompanyApi.selection Company
        |> with CompanyApi.name


user : SelectionSet User Api.User
user =
    UserApi.selection User
        |> with UserApi.firstName
        |> with UserApi.lastName
        |> with (UserApi.company company)


query : SelectionSet User RootQuery
query =
    Query.selection identity
        |> with (Query.me user)


getProfile : Api.ProtectedConfig -> (Api.ApiResult User -> msg) -> Cmd msg
getProfile apiConfig msg =
    Api.sendGraphQL apiConfig msg query
