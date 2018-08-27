module App.Pages.Environments.Copy.Data exposing
    ( Project
    , listProjects
    )

import Api
import Api.Object as Api
import Api.Object.Project as ProjectApi
import Api.Query as Query
import Graphqelm.Operation exposing (RootQuery)
import Graphqelm.SelectionSet exposing (SelectionSet, hardcoded, with)


type alias Project =
    { id : String
    , name : String
    }


project : SelectionSet Project Api.Project
project =
    ProjectApi.selection Project
        |> with ProjectApi.id
        |> with ProjectApi.name


query : SelectionSet (List Project) RootQuery
query =
    Query.selection identity
        |> with (Query.projects project)


listProjects : String -> String -> (Api.ApiResult (List Project) -> msg) -> Cmd msg
listProjects baseUrl token msg =
    Api.send baseUrl token msg query
