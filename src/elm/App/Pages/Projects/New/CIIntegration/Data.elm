module App.Pages.Projects.New.CIIntegration.Data exposing
    ( Project
    , getData
    )

import Api
import Api.Object as Api
import Api.Object.Project as ProjectApi
import Api.Query as Query
import Graphqelm.Operation exposing (RootQuery)
import Graphqelm.SelectionSet exposing (SelectionSet, with)


type alias Project =
    { id : String
    , accessToken : String
    }


project : SelectionSet Project Api.Project
project =
    ProjectApi.selection Project
        |> with ProjectApi.id
        |> with ProjectApi.accessToken


query : String -> SelectionSet (Maybe Project) RootQuery
query projectId =
    Query.selection identity
        |> with (Query.project { id = projectId } project)


getData : String -> String -> String -> (Api.ApiResult (Maybe Project) -> msg) -> Cmd msg
getData baseUrl token projectId msg =
    Api.send baseUrl token msg (query projectId)
