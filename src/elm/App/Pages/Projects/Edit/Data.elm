module App.Pages.Projects.Edit.Data exposing
    ( Project
    , Response
    , SlackAccessToken
    , SlackConfiguration
    , getData
    )

import Api
import Api.Object as Api
import Api.Object.Project as ProjectApi
import Api.Object.SlackAccessToken as SlackAccessTokenApi
import Api.Object.SlackChannel as SlackChannelApi
import Api.Object.SlackConfiguration as SlackConfigurationApi
import Api.Object.SlackProjectIntegration as SlackProjectIntegrationApi
import Api.Query as Query
import Graphql.Operation exposing (RootQuery)
import Graphql.SelectionSet exposing (SelectionSet, with)


type alias SlackAccessToken =
    { teamName : String
    , channels : List SlackChannel
    }


type alias SlackChannel =
    { key : String
    , val : String
    }


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
    { channelId : String }


type alias Response =
    { slackConfiguration : SlackConfiguration
    , slackAccessToken : Maybe SlackAccessToken
    , project : Maybe Project
    }


slackChannel : SelectionSet SlackChannel Api.SlackChannel
slackChannel =
    SlackChannelApi.selection SlackChannel
        |> with SlackChannelApi.id
        |> with SlackChannelApi.name


slackAccessToken : SelectionSet SlackAccessToken Api.SlackAccessToken
slackAccessToken =
    SlackAccessTokenApi.selection SlackAccessToken
        |> with SlackAccessTokenApi.teamName
        |> with (SlackAccessTokenApi.channels slackChannel)


slackIntegration : SelectionSet SlackIntegration Api.SlackProjectIntegration
slackIntegration =
    SlackProjectIntegrationApi.selection SlackIntegration
        |> with SlackProjectIntegrationApi.channelId


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
        |> with (Query.slackAccessToken slackAccessToken)
        |> with (Query.project { id = projectId } project)


getData : Api.ProtectedConfig -> String -> (Api.ApiResult Response -> msg) -> Cmd msg
getData apiConfig projectId msg =
    Api.sendGraphQL apiConfig msg (query projectId)
