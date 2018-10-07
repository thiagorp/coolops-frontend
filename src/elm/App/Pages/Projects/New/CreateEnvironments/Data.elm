module App.Pages.Projects.New.CreateEnvironments.Data exposing
    ( Environment
    , Project
    , Response
    , getData
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
    { name : String
    }


type alias Project =
    { environments : List Environment
    }


type alias Response =
    { currentProject : Maybe Project
    , formData : FormData.Response
    }


param : SelectionSet ( String, String ) Api.Param
param =
    ParamApi.selection (\a b -> ( a, b ))
        |> with ParamApi.key
        |> with ParamApi.value


environment : SelectionSet Environment Api.Environment
environment =
    EnvironmentApi.selection Environment
        |> with EnvironmentApi.name


project : SelectionSet Project Api.Project
project =
    ProjectApi.selection Project
        |> with (ProjectApi.environments environment)


query : String -> SelectionSet Response RootQuery
query projectId =
    Query.selection Response
        |> with (Query.project { id = projectId } project)
        |> with FormData.query


getData : Api.ProtectedConfig -> String -> (Api.ApiResult Response -> msg) -> Cmd msg
getData apiConfig projectId msg =
    Api.sendGraphQL apiConfig msg (query projectId)
