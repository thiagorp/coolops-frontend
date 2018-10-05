module App.Pages.Projects.New.SlackIntegration.Main exposing
    ( Model
    , Msg
    , init
    , update
    , view
    )

import Api
import App.Pages.Projects.New.SlackIntegration.Data as Data
import Html exposing (..)
import Html.Attributes exposing (..)
import RemoteData exposing (RemoteData(..))
import Route
import Util exposing (PageHandler, andPerform, noop, return)


type alias Model =
    { apiConfig : Api.ProtectedConfig
    , apiData : Api.ApiData Data.Response
    , error : Bool
    , navigationKey : Route.NavigationKey
    }


type Msg
    = DataLoaded (Api.ApiResult Data.Response)


init : Api.ProtectedConfig -> Route.NavigationKey -> String -> Bool -> PageHandler Model Msg
init apiConfig navigationKey projectId error =
    return
        { apiConfig = apiConfig
        , error = error
        , apiData = Loading
        , navigationKey = navigationKey
        }
        |> andPerform (Data.getData apiConfig projectId DataLoaded)


update : Msg -> Model -> PageHandler Model Msg
update msg model =
    case msg of
        DataLoaded (Ok result) ->
            case result.project of
                Just project ->
                    return { model | apiData = Success result }

                Nothing ->
                    return model
                        |> andPerform (Route.redirectTo model.navigationKey (Route.Protected (Route.NewProject Route.CreateProject)))

        DataLoaded (Err e) ->
            return { model | apiData = Failure e }


slackUri : String -> String -> String
slackUri slackClientId projectId =
    "https://slack.com/oauth/authorize?client_id=" ++ slackClientId ++ "&scope=chat:write,commands&single_channel=true&state=newProject|" ++ projectId


errorLine : Model -> Html msg
errorLine { error } =
    if error then
        div [ class "alert alert-danger mt-4" ] [ text "There was an error integrating with Slack. Please try again." ]

    else
        div [] []


view : Model -> Html Msg
view model =
    case model.apiData of
        NotAsked ->
            div [] []

        Loading ->
            div [] []

        Success response ->
            case response.project of
                Nothing ->
                    div [] [ text "Project not found" ]

                Just project ->
                    div [ class "card" ]
                        [ div [ class "card-body" ]
                            [ p [] [ text "Click the button below to allow CoolOps to post notifications about the project to a Slack channel" ]
                            , a [ href (slackUri response.slackConfiguration.clientId project.id) ]
                                [ img
                                    [ alt "Add to Slack"
                                    , src "https://platform.slack-edge.com/img/add_to_slack.png"
                                    , attribute "srcset" "https://platform.slack-edge.com/img/add_to_slack.png 1x, https://platform.slack-edge.com/img/add_to_slack@2x.png 2x"
                                    ]
                                    []
                                ]
                            , errorLine model
                            ]
                        ]

        Failure e ->
            div [] [ text "Failed to load data" ]
