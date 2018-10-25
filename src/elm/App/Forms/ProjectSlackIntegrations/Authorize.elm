module App.Forms.ProjectSlackIntegrations.Authorize exposing
    ( Model
    , Msg
    , init
    , update
    , view
    )

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Route
import Util exposing (PageHandler, andPerform, noop, return)



-- Model


type alias Model =
    { error : Bool
    , navigationKey : Route.NavigationKey
    , projectId : String
    , slackClientId : String
    , statePrefix : String
    }



-- Msg


type Msg
    = Noop



-- Init


type alias SlackConfiguration a =
    { a | clientId : String }


type alias Project a =
    { a | id : String }


init : Route.NavigationKey -> SlackConfiguration a -> Project b -> Bool -> String -> PageHandler Model Msg
init navigationKey slackConfig project error statePrefix =
    return
        { error = error
        , navigationKey = navigationKey
        , projectId = project.id
        , slackClientId = slackConfig.clientId
        , statePrefix = statePrefix
        }



-- Update


update : Msg -> Model -> PageHandler Model Msg
update msg model =
    case msg of
        Noop ->
            return model



-- View


slackUri : Model -> String
slackUri { slackClientId, projectId, statePrefix } =
    "https://slack.com/oauth/authorize?client_id=" ++ slackClientId ++ "&scope=commands,chat:write:bot,bot&single_channel=true&state=" ++ statePrefix ++ projectId


errorLine : Model -> Html msg
errorLine { error } =
    if error then
        div [ class "alert alert-danger mt-4" ] [ text "There was an error integrating with Slack. Please try again." ]

    else
        div [] []


view : Model -> List (Html Msg)
view model =
    [ p [] [ text "It seems like you haven't synced any project with Slack yet. Click the button below to authorize coolops on your workspace." ]
    , a [ href (slackUri model) ]
        [ img
            [ alt "Add to Slack"
            , src "https://platform.slack-edge.com/img/add_to_slack.png"
            , attribute "srcset" "https://platform.slack-edge.com/img/add_to_slack.png 1x, https://platform.slack-edge.com/img/add_to_slack@2x.png 2x"
            ]
            []
        ]
    , errorLine model
    ]
