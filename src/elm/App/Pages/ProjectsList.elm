module App.Pages.ProjectsList exposing (..)

import App.Api.ListProjects exposing (..)
import App.Html as AppHtml exposing (spinner)
import Date
import Date.Distance as Distance
import Html exposing (..)
import Html.Attributes exposing (..)
import Http exposing (Error)
import RemoteData exposing (RemoteData(..), WebData)
import Route exposing (..)
import Task exposing (..)
import Time exposing (Time)
import Util exposing (PageHandler, andPerform, noop, return)


type alias Model =
    { projects : WebData (List Project), apiToken : String, loadedTime : Maybe Time }


init : String -> PageHandler Model Msg
init apiToken =
    return { projects = Success projects, apiToken = apiToken, loadedTime = Nothing }
        |> andPerform (Task.perform TimeLoaded Time.now)
        |> andPerform (listProjects apiToken ProjectsResponse)


type Msg
    = ProjectsResponse (Result Error (List Project))
    | CreateNewButtonClicked
    | LinkClicked Route
    | TimeLoaded Time


update : Msg -> Model -> PageHandler Model Msg
update msg model =
    case msg of
        LinkClicked route ->
            return model
                |> andPerform (redirectTo route)

        ProjectsResponse result ->
            { model | projects = RemoteData.fromResult result }
                |> return

        CreateNewButtonClicked ->
            return model
                |> andPerform (redirectTo (Protected NewProject))

        TimeLoaded time ->
            { model | loadedTime = Just time }
                |> return


projects : List Project
projects =
    [ { id = "core"
      , name = "Availability Service"
      , deploymentImage = "docker/hello"
      , environments =
            [ { id = "core-produciton"
              , name = "Production"
              , currentDeployment =
                    Just
                        { id = "lalala"
                        , build = { id = "lala", name = "master-512" }
                        , startedAt = 1530403751710
                        , status = Failed
                        }
              }
            , { id = "core-staging"
              , name = "Staging"
              , currentDeployment =
                    Just
                        { id = "lalala"
                        , build = { id = "lala", name = "very-long-branch-name-that-is-almost-impossible-4" }
                        , startedAt = 1530423751710
                        , status = Running
                        }
              }
            ]
      }
    , { id = "core"
      , name = "Core"
      , deploymentImage = "docker/hello"
      , environments =
            [ { id = "core-produciton"
              , name = "Production"
              , currentDeployment =
                    Just
                        { id = "lalala"
                        , build = { id = "lala", name = "master-4" }
                        , startedAt = 1530473752710
                        , status = Succeeded
                        }
              }
            , { id = "core-staging"
              , name = "Staging"
              , currentDeployment = Nothing
              }
            ]
      }
    , { id = "core"
      , name = "Customers Service"
      , deploymentImage = "docker/hello"
      , environments =
            [ { id = "core-produciton"
              , name = "Production"
              , currentDeployment =
                    Just
                        { id = "lalala"
                        , build = { id = "lala", name = "master-10" }
                        , startedAt = 1530403751710
                        , status = Succeeded
                        }
              }
            , { id = "core-staging"
              , name = "Staging"
              , currentDeployment =
                    Just
                        { id = "lalala"
                        , build = { id = "lala", name = "feature-branch-nice-feature-1" }
                        , startedAt = 1530423751710
                        , status = Queued
                        }
              }
            ]
      }
    , { id = "core"
      , name = "Reminder Service"
      , deploymentImage = "docker/hello"
      , environments = []
      }
    ]


deploymentCol : Maybe Time -> Maybe Deployment -> List (Html msg)
deploymentCol maybeCurrentTime maybeDeploment =
    let
        timeText deploymentTime =
            case maybeCurrentTime of
                Nothing ->
                    text ""

                Just time ->
                    Distance.inWords (Date.fromTime time) (Date.fromTime deploymentTime)
                        |> (++) "Last update: "
                        |> (\s -> s ++ " ago")
                        |> text
    in
    case maybeDeploment of
        Nothing ->
            [ span [ class "text-muted" ] [ text "Haven't been deployed yet" ] ]

        Just deployment ->
            [ div []
                [ div []
                    [ text deployment.build.name
                    ]
                , div [ class "small text-muted" ]
                    [ timeText deployment.startedAt ]
                ]
            ]


deploymentStatusCol : Maybe Deployment -> List (Html msg)
deploymentStatusCol maybeDeploment =
    let
        statusBg deployment =
            case deployment.status of
                Queued ->
                    "bg-secondary"

                Running ->
                    "bg-warning"

                Succeeded ->
                    "bg-success"

                Failed ->
                    "bg-danger"
    in
    case maybeDeploment of
        Nothing ->
            []

        Just deployment ->
            [ span [ class "status-icon", class (statusBg deployment) ] [] ]


environmentRow : Maybe Time -> Environment -> Html msg
environmentRow maybeCurrentTime environment =
    tr []
        [ td [ width 1, class "pr-0" ] (deploymentStatusCol environment.currentDeployment)
        , td [] [ text environment.name ]
        , td [] (deploymentCol maybeCurrentTime environment.currentDeployment)
        ]


projectTableBody : Maybe Time -> Project -> List (Html Msg)
projectTableBody maybeCurrentTime project =
    case project.environments of
        [] ->
            [ td [ colspan 3, class "text-center" ] [ i [ class "text-muted" ] [ text "No environment yet" ] ] ]

        _ ->
            List.map (environmentRow maybeCurrentTime) project.environments


projectTable : Maybe Time -> Project -> Html Msg
projectTable maybeCurrentTime project =
    div [ class "table-responsive" ]
        [ table [ class "table table-hover table-outline table-vcenter card-table" ]
            [ thead []
                [ tr []
                    [ th [] []
                    , th [] [ text "Environment name" ]
                    , th [] [ text "Current deployment" ]
                    ]
                ]
            , tbody [] (projectTableBody maybeCurrentTime project)
            ]
        ]


projectView : Maybe Time -> Project -> Html Msg
projectView maybeCurrentTime project =
    div [ class "row row-cards row-deck" ]
        [ div [ class "col-12" ]
            [ div [ class "card" ]
                [ div [ class "card-header" ]
                    [ h3 [ class "card-title" ] [ text project.name ]
                    , div [ class "card-options" ]
                        [ AppHtml.a (Protected (EditProject project.id))
                            LinkClicked
                            [ class "btn btn-sm btn-secondary" ]
                            [ i [ class "fas fa-cog mr-2" ] [], text "Settings" ]
                        , AppHtml.a (Protected (NewEnvironment project.id))
                            LinkClicked
                            [ class "btn btn-sm btn-success ml-2" ]
                            [ i [ class "fas fa-plus mr-2" ] [], text "Environment" ]
                        ]
                    ]
                , projectTable maybeCurrentTime project
                ]
            ]
        ]


content : Model -> Html Msg
content { projects, loadedTime } =
    case projects of
        NotAsked ->
            spinner

        Loading ->
            spinner

        Success projects ->
            List.map (projectView loadedTime) projects
                |> div []

        Failure e ->
            text (toString e)


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ div [ class "page-header" ]
            [ h1 [ class "page-title" ] [ text "Projects" ]
            , div [ class "page-options d-flex" ]
                [ AppHtml.a (Protected NewProject) LinkClicked [ class "btn btn-success" ] [ i [ class "fas fa-plus mr-2" ] [], text "Project" ] ]
            ]
        , content model
        ]
