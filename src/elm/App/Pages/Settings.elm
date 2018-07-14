module App.Pages.Settings exposing (..)

import App.Api.DisconnectSlack exposing (..)
import App.Api.GetSlackConfig exposing (..)
import App.Api.HandleSlackOAuthCallback exposing (..)
import App.Html exposing (..)
import Html exposing (Html)
import Html.Attributes as Attr
import Http exposing (Error)
import RemoteData exposing (RemoteData(..), WebData)
import Util exposing (..)


type Msg
    = SlackConfigResponse (Result Error SlackConfig)
    | SlackOAuthPostResponse (Result Error ())
    | DisconnectSlack
    | DisconnectSlackResponse (Result Error ())


type alias Model =
    { apiToken : String
    , slackConfig : WebData SlackConfig
    , baseUrl : String
    }


init : String -> String -> Maybe String -> PageHandler Model Msg
init baseUrl apiToken maybeCode =
    return (Model apiToken Loading baseUrl)
        |> andPerform (handleSlack baseUrl apiToken maybeCode)


handleSlack : String -> String -> Maybe String -> Cmd Msg
handleSlack baseUrl apiToken maybeCode =
    case maybeCode of
        Nothing ->
            getSlackConfig baseUrl apiToken SlackConfigResponse

        Just code ->
            handleSlackOAuthCallback baseUrl apiToken SlackOAuthPostResponse { code = code }


update : Msg -> Model -> PageHandler Model Msg
update msg model =
    case msg of
        SlackConfigResponse result ->
            { model | slackConfig = RemoteData.fromResult result }
                |> return

        SlackOAuthPostResponse _ ->
            return model
                |> andPerform (getSlackConfig model.baseUrl model.apiToken SlackConfigResponse)

        DisconnectSlack ->
            { model | slackConfig = Loading }
                |> return
                |> andPerform (disconnectSlack model.baseUrl model.apiToken DisconnectSlackResponse)

        DisconnectSlackResponse _ ->
            return model
                |> andPerform (getSlackConfig model.baseUrl model.apiToken SlackConfigResponse)


slackUrl : SlackConfig -> String
slackUrl { clientId } =
    "https://slack.com/oauth/authorize?client_id=" ++ clientId ++ "&scope=incoming-webhook,chat:write:bot,commands,bot"


slackSettings : Model -> Html Msg
slackSettings { slackConfig } =
    case slackConfig of
        NotAsked ->
            spinner

        Loading ->
            spinner

        Success config ->
            case config.enabled of
                True ->
                    { buttonConfig | color = Danger, icon = Just Slack, text = "Disconnect" }
                        |> button DisconnectSlack

                False ->
                    externalLink (slackUrl config)
                        []
                        [ img "https://platform.slack-edge.com/img/add_to_slack.png"
                            [ Attr.attribute "srcset" "https://platform.slack-edge.com/img/add_to_slack.png 1x, https://platform.slack-edge.com/img/add_to_slack@2x.png 2x" ]
                        ]

        Failure e ->
            text (toString e)


view : Model -> Html Msg
view model =
    container
        [ pageHeader "Settings"
        , cardWithTitle "Slack integration"
            [ cardBody [ slackSettings model ] ]
        ]
