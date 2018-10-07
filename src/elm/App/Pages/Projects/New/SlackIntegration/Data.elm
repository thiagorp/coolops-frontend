module App.Pages.Projects.New.SlackIntegration.Data exposing
    ( Project
    , Response
    , SlackConfiguration
    , getData
    )

import Api
import Api.Object as Api
import Api.Object.Project as ProjectApi
import Api.Object.SlackConfiguration as SlackConfigurationApi
import Api.Query as Query
import Graphql.Operation exposing (RootQuery)
import Graphql.SelectionSet exposing (SelectionSet, with)


type alias SlackConfiguration =
    { clientId : String }


type alias Project =
    { id : String }


type alias Response =
    { slackConfiguration : SlackConfiguration
    , project : Maybe Project
    }


project : SelectionSet Project Api.Project
project =
    ProjectApi.selection Project
        |> with ProjectApi.id


slackConfiguration : SelectionSet SlackConfiguration Api.SlackConfiguration
slackConfiguration =
    SlackConfigurationApi.selection SlackConfiguration
        |> with SlackConfigurationApi.clientId


query : String -> SelectionSet Response RootQuery
query projectId =
    Query.selection Response
        |> with (Query.slackConfiguration slackConfiguration)
        |> with (Query.project { id = projectId } project)


getData : Api.ProtectedConfig -> String -> (Api.ApiResult Response -> msg) -> Cmd msg
getData apiConfig projectId msg =
    Api.sendGraphQL apiConfig msg (query projectId)
