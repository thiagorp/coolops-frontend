module App.Pages.Projects.New.SlackIntegration.Data exposing
    ( Project
    , Response
    , SlackAccessToken
    , SlackChannel
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


type alias SlackIntegration =
    { channelId : String
    }


type alias Project =
    { id : String
    , slackIntegration : Maybe SlackIntegration
    }


type alias Response =
    { project : Maybe Project
    , slackAccessToken : Maybe SlackAccessToken
    , slackConfiguration : SlackConfiguration
    }


slackIntegration : SelectionSet SlackIntegration Api.SlackProjectIntegration
slackIntegration =
    SlackProjectIntegrationApi.selection SlackIntegration
        |> with SlackProjectIntegrationApi.channelId


project : SelectionSet Project Api.Project
project =
    ProjectApi.selection Project
        |> with ProjectApi.id
        |> with (ProjectApi.slackIntegration slackIntegration)


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


slackConfiguration : SelectionSet SlackConfiguration Api.SlackConfiguration
slackConfiguration =
    SlackConfigurationApi.selection SlackConfiguration
        |> with SlackConfigurationApi.clientId


query : String -> SelectionSet Response RootQuery
query projectId =
    Query.selection Response
        |> with (Query.project { id = projectId } project)
        |> with (Query.slackAccessToken slackAccessToken)
        |> with (Query.slackConfiguration slackConfiguration)


getData : Api.ProtectedConfig -> String -> (Api.ApiResult Response -> msg) -> Cmd msg
getData apiConfig projectId msg =
    Api.sendGraphQL apiConfig msg (query projectId)
