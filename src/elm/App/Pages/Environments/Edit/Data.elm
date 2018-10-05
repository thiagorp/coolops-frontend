module App.Pages.Environments.Edit.Data exposing
    ( Environment
    , Response
    , getEnvironment
    )

import Api
import Api.Object as Api
import Api.Object.Environment as EnvironmentApi
import Api.Object.Param as ParamApi
import Api.Object.Project as ProjectApi
import Api.Query as Query
import App.Forms.Environments.Data as FormData
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
    , slug : String
    , environmentVars : Dict String String
    , projectId : String
    }


type alias Response =
    { environment : Maybe Environment
    , formData : FormData.Response
    }


param : SelectionSet ( String, String ) Api.Param
param =
    ParamApi.selection (\a b -> ( a, b ))
        |> with ParamApi.key
        |> with ParamApi.value


project : SelectionSet String Api.Project
project =
    ProjectApi.selection identity
        |> with ProjectApi.id


environment : SelectionSet Environment Api.Environment
environment =
    EnvironmentApi.selection Environment
        |> with EnvironmentApi.id
        |> with EnvironmentApi.name
        |> with EnvironmentApi.slug
        |> with
            (EnvironmentApi.environmentVariables param
                |> Field.map Dict.fromList
            )
        |> with (EnvironmentApi.project project)


query : String -> SelectionSet Response RootQuery
query id =
    Query.selection Response
        |> with (Query.environment { id = id } environment)
        |> with FormData.query


getEnvironment : Api.ProtectedConfig -> String -> (Api.ApiResult Response -> msg) -> Cmd msg
getEnvironment apiConfig id msg =
    Api.sendGraphQL apiConfig msg (query id)
