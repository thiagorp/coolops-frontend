module App.Pages.Projects.Edit.Data exposing
    ( Project
    , Response
    , SlackConfiguration
    , getData
    )

import Api
import Api.Object as Api
import Api.Object.Project as ProjectApi
import Api.Object.SlackConfiguration as SlackConfigurationApi
import Api.Object.SlackProjectIntegration as SlackProjectIntegrationApi
import Api.Query as Query
import Graphql.Operation exposing (RootQuery)
import Graphql.SelectionSet exposing (SelectionSet, with)


type alias SlackConfiguration =
    { clientId : String }


type alias Project =
    { id : String
    , name : String
    , slug : String
    , deploymentImage : String
    , accessToken : String
    , slackIntegration : Maybe SlackIntegration
    }


type alias SlackIntegration =
    { workspaceName : String }


type alias Response =
    { slackConfiguration : SlackConfiguration
    , project : Maybe Project
    }


slackIntegration : SelectionSet SlackIntegration Api.SlackProjectIntegration
slackIntegration =
    SlackProjectIntegrationApi.selection SlackIntegration
        |> with SlackProjectIntegrationApi.workspaceName


project : SelectionSet Project Api.Project
project =
    ProjectApi.selection Project
        |> with ProjectApi.id
        |> with ProjectApi.name
        |> with ProjectApi.slug
        |> with ProjectApi.deploymentImage
        |> with ProjectApi.accessToken
        |> with (ProjectApi.slackIntegration slackIntegration)


slackConfiguration : SelectionSet SlackConfiguration Api.SlackConfiguration
slackConfiguration =
    SlackConfigurationApi.selection SlackConfiguration
        |> with SlackConfigurationApi.clientId


query : String -> SelectionSet Response RootQuery
query projectId =
    Query.selection Response
        |> with (Query.slackConfiguration slackConfiguration)
        |> with (Query.project { id = projectId } project)


getData : String -> String -> String -> (Api.ApiResult Response -> msg) -> Cmd msg
getData baseUrl token projectId msg =
    Api.send baseUrl token msg (query projectId)
