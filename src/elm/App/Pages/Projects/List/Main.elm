module App.Pages.Projects.List.Main exposing
    ( Model
    , Msg(..)
    , init
    , update
    , view
    )

import Api exposing (ApiData, ApiResult)
import Api.Enum.DeploymentStatus exposing (DeploymentStatus(..))
import App.Html as AppHtml exposing (spinner)
import App.Pages.Projects.List.Data exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import RemoteData exposing (RemoteData(..), WebData)
import Route exposing (..)
import Task exposing (..)
import Time
import Util exposing (PageHandler, andPerform, noop, return)


type alias Model =
    { projects : ApiData (List Project)
    , apiConfig : Api.ProtectedConfig
    , loadedTime : Maybe Time.Posix
    }


init : Api.ProtectedConfig -> PageHandler Model Msg
init apiConfig =
    return { projects = Loading, apiConfig = apiConfig, loadedTime = Nothing }
        |> andPerform (Task.perform TimeLoaded Time.now)
        |> andPerform (listProjects apiConfig ProjectsResponse)


type Msg
    = ProjectsResponse (ApiResult (List Project))
    | TimeLoaded Time.Posix


update : Msg -> Model -> PageHandler Model Msg
update msg model =
    case msg of
        ProjectsResponse result ->
            { model | projects = RemoteData.fromResult result }
                |> return

        TimeLoaded time ->
            { model | loadedTime = Just time }
                |> return


deploymentCol : Maybe Time.Posix -> Maybe Deployment -> List (Html msg)
deploymentCol maybeCurrentTime maybeDeploment =
    let
        timeText deploymentTime =
            case maybeCurrentTime of
                Nothing ->
                    text ""

                Just time ->
                    case deploymentTime of
                        Nothing ->
                            text "Queued"

                        Just t ->
                            text "Deployed"

        -- Distance.inWords (Date.fromTime time) (Date.fromTime t)
        -- |> (++) "Deployed: "
        -- |> (\s -> s ++ " ago")
        -- |> text
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


environmentRow : Maybe Time.Posix -> Environment -> Html Msg
environmentRow maybeCurrentTime environment =
    tr []
        [ td [ width 1, class "pr-0" ] (deploymentStatusCol environment.currentDeployment)
        , td []
            [ div []
                [ div [] [ text environment.name ]
                , div [ class "small text-muted" ] [ text ("ID: " ++ environment.id) ]
                ]
            ]
        , td [] (deploymentCol maybeCurrentTime environment.currentDeployment)
        , td [ class "text-right" ]
            [ a
                [ class "icon", title "Edit", href (toUrl (Protected (EditEnvironment environment.id))) ]
                [ i [ class "fe fe-edit" ] [] ]
            ]
        ]


projectTableBody : Maybe Time.Posix -> Project -> List (Html Msg)
projectTableBody maybeCurrentTime project =
    case project.environments of
        [] ->
            [ td [ colspan 4, class "text-center" ] [ i [ class "text-muted" ] [ text "No environment yet" ] ] ]

        _ ->
            List.map (environmentRow maybeCurrentTime) project.environments


projectTable : Maybe Time.Posix -> Project -> Html Msg
projectTable maybeCurrentTime project =
    div [ class "table-responsive" ]
        [ table [ class "table table-hover table-outline table-vcenter card-table" ]
            [ thead []
                [ tr []
                    [ th [] []
                    , th [] [ text "Environment name" ]
                    , th [] [ text "Current deployment" ]
                    , th [] []
                    ]
                ]
            , tbody [] (projectTableBody maybeCurrentTime project)
            ]
        ]


projectView : Maybe Time.Posix -> Project -> Html Msg
projectView maybeCurrentTime project =
    div [ class "row row-cards row-deck" ]
        [ div [ class "col-12" ]
            [ div [ class "card" ]
                [ div [ class "card-header" ]
                    [ h3 [ class "card-title" ] [ text project.name ]
                    , div [ class "card-options" ]
                        [ a
                            [ class "btn btn-sm btn-secondary", href (toUrl (Protected (EditProject project.id))) ]
                            [ i [ class "fas fa-cog mr-2" ] [], text "Settings" ]
                        , a
                            [ class "btn btn-sm btn-success ml-2", href (toUrl (Protected (NewEnvironment project.id))) ]
                            [ i [ class "fas fa-plus mr-2" ] [], text "Environment" ]
                        ]
                    ]
                , projectTable maybeCurrentTime project
                ]
            ]
        ]


content : Model -> Html Msg
content model =
    case model.projects of
        NotAsked ->
            spinner

        Loading ->
            spinner

        Success projects ->
            List.map (projectView model.loadedTime) projects
                |> div []

        Failure e ->
            text "Something bad happened"


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ div [ class "page-header" ]
            [ h1 [ class "page-title" ] [ text "Projects" ]
            , div [ class "page-options d-flex" ]
                [ a
                    [ class "btn btn-success", href (toUrl (Protected (NewProject CreateProject))) ]
                    [ i [ class "fas fa-plus mr-2" ] [], text "Project" ]
                ]
            ]
        , content model
        ]
