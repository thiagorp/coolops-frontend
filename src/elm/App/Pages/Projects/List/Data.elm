module App.Pages.Projects.List.Data exposing
    ( Build
    , Deployment
    , Environment
    , Project
    , listProjects
    )

import Api
import Api.Enum.DeploymentStatus exposing (DeploymentStatus(..))
import Api.Object as Api
import Api.Object.Build as BuildApi
import Api.Object.Deployment as DeploymentApi
import Api.Object.Environment as EnvironmentApi
import Api.Object.Project as ProjectApi
import Api.Query as Query
import Graphqelm.Field as Field
import Graphqelm.Operation exposing (RootQuery)
import Graphqelm.SelectionSet exposing (SelectionSet, with)
import Time exposing (Time)


type alias Build =
    { id : String
    , name : String
    }


type alias Deployment =
    { id : String
    , build : Build
    , startedAt : Maybe Time
    , status : DeploymentStatus
    }


type alias Environment =
    { id : String
    , name : String
    , currentDeployment : Maybe Deployment
    }


type alias Project =
    { id : String
    , name : String
    , deploymentImage : String
    , environments : List Environment
    }


build : SelectionSet Build Api.Build
build =
    BuildApi.selection Build
        |> with BuildApi.id
        |> with BuildApi.name


deployment : SelectionSet Deployment Api.Deployment
deployment =
    DeploymentApi.selection Deployment
        |> with DeploymentApi.id
        |> with (DeploymentApi.build build)
        |> with
            (DeploymentApi.startedAt
                |> Field.map
                    (Maybe.map (toFloat >> (*) Time.second))
            )
        |> with DeploymentApi.status


environment : SelectionSet Environment Api.Environment
environment =
    EnvironmentApi.selection Environment
        |> with EnvironmentApi.id
        |> with EnvironmentApi.name
        |> with (EnvironmentApi.lastDeployment deployment)


project : SelectionSet Project Api.Project
project =
    ProjectApi.selection Project
        |> with ProjectApi.id
        |> with ProjectApi.name
        |> with ProjectApi.deploymentImage
        |> with (ProjectApi.environments environment)


query : SelectionSet (List Project) RootQuery
query =
    Query.selection identity
        |> with (Query.projects project)


listProjects : String -> String -> (Api.ApiResult (List Project) -> msg) -> Cmd msg
listProjects baseUrl token msg =
    Api.send baseUrl token msg query
