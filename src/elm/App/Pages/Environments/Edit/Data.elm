module App.Pages.Environments.Edit.Data exposing
    ( Environment
    , getEnvironment
    )

import Api
import Api.Object as Api
import Api.Object.Environment as EnvironmentApi
import Api.Object.Param as ParamApi
import Api.Query as Query
import Dict exposing (Dict)
import Graphql.Field as Field
import Graphql.Operation exposing (RootQuery)
import Graphql.SelectionSet exposing (SelectionSet, with)


type alias Param =
    { key : String
    , value : String
    }


type alias Environment =
    { id : String
    , name : String
    , environmentVars : Dict String String
    }


param : SelectionSet ( String, String ) Api.Param
param =
    ParamApi.selection (\a b -> ( a, b ))
        |> with ParamApi.key
        |> with ParamApi.value


environment : SelectionSet Environment Api.Environment
environment =
    EnvironmentApi.selection Environment
        |> with EnvironmentApi.id
        |> with EnvironmentApi.name
        |> with
            (EnvironmentApi.environmentVariables param
                |> Field.map Dict.fromList
            )


query : String -> SelectionSet (Maybe Environment) RootQuery
query id =
    Query.selection identity
        |> with (Query.environment { id = id } environment)


getEnvironment : String -> String -> String -> (Api.ApiResult (Maybe Environment) -> msg) -> Cmd msg
getEnvironment baseUrl token id msg =
    Api.send baseUrl token msg (query id)
