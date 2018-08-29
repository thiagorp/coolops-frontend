module App.Pages.Environments.Copy.Data exposing
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
import Dict exposing (Dict)
import Graphqelm.Field as Field
import Graphqelm.Operation exposing (RootQuery)
import Graphqelm.SelectionSet exposing (SelectionSet, hardcoded, with)


type alias Project =
    { id : String
    , name : String
    }


type alias Environment =
    { projectId : String
    , environmentVars : Dict String String
    }


type alias Response =
    { projects : List Project
    , environment : Maybe Environment
    }


param : SelectionSet ( String, String ) Api.Param
param =
    ParamApi.selection (,)
        |> with ParamApi.key
        |> with ParamApi.value


project : SelectionSet Project Api.Project
project =
    ProjectApi.selection Project
        |> with ProjectApi.id
        |> with ProjectApi.name


environmentProject : SelectionSet String Api.Project
environmentProject =
    ProjectApi.selection identity
        |> with ProjectApi.id


environment : SelectionSet Environment Api.Environment
environment =
    EnvironmentApi.selection Environment
        |> with (EnvironmentApi.project environmentProject)
        |> with (EnvironmentApi.environmentVariables param |> Field.map Dict.fromList)


query : String -> SelectionSet Response RootQuery
query id =
    Query.selection Response
        |> with (Query.projects project)
        |> with (Query.environment { id = id } environment)


getData : String -> String -> String -> (Api.ApiResult Response -> msg) -> Cmd msg
getData baseUrl token id msg =
    query id
        |> Api.send baseUrl token msg
