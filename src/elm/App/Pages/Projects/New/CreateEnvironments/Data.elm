module App.Pages.Projects.New.CreateEnvironments.Data exposing
    ( Environment
    , EnvironmentForEnvironmentVars
    , Project
    , ProjectForEnvironmentVars
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
import Graphqelm.SelectionSet exposing (SelectionSet, with)


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


type alias EnvironmentForEnvironmentVars =
    { id : String
    , name : String
    , environmentVars : Dict String String
    }


type alias ProjectForEnvironmentVars =
    { name : String
    , environments : List EnvironmentForEnvironmentVars
    }


type alias Response =
    { currentProject : Maybe Project
    , allProjects : List ProjectForEnvironmentVars
    }


param : SelectionSet ( String, String ) Api.Param
param =
    ParamApi.selection (,)
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


environmentForEnvironmentVars : SelectionSet EnvironmentForEnvironmentVars Api.Environment
environmentForEnvironmentVars =
    EnvironmentApi.selection EnvironmentForEnvironmentVars
        |> with EnvironmentApi.id
        |> with EnvironmentApi.name
        |> with
            (EnvironmentApi.environmentVariables param
                |> Field.map Dict.fromList
            )


projectForEnvironmentVars : SelectionSet ProjectForEnvironmentVars Api.Project
projectForEnvironmentVars =
    ProjectApi.selection ProjectForEnvironmentVars
        |> with ProjectApi.name
        |> with (ProjectApi.environments environmentForEnvironmentVars)


query : String -> SelectionSet Response RootQuery
query projectId =
    Query.selection Response
        |> with (Query.project { id = projectId } project)
        |> with (Query.projects projectForEnvironmentVars)


getData : String -> String -> String -> (Api.ApiResult Response -> msg) -> Cmd msg
getData baseUrl token projectId msg =
    Api.send baseUrl token msg (query projectId)
