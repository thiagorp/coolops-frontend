module App.Main exposing (..)

import App.Api.GetEnvironment exposing (Environment, getEnvironment)
import App.Api.GetProject exposing (Project, getProject)
import App.Fragments.Topbar as Topbar
import App.Pages.EditEnvironment as EditEnvironment
import App.Pages.EditProject as EditProject
import App.Pages.NewEnvironment as NewEnvironment
import App.Pages.NewProject as NewProject
import App.Pages.NotFound as NotFound
import App.Pages.ProjectsList as ProjectsList
import App.Pages.Settings as Settings
import Html
import Http
import Navigation
import Route
import Util


type Msg
    = UrlChanged Navigation.Location
    | ProjectsListMsg ProjectsList.Msg
    | NewProjectMsg NewProject.Msg
    | EditProjectMsg EditProject.Msg
    | NewEnvironmentMsg NewEnvironment.Msg
    | EditEnvironmentMsg EditEnvironment.Msg
    | SettingsMsg Settings.Msg
    | TopbarMsg Topbar.Msg
    | ProjectLoaded ProjectScopedPage (Result Http.Error Project)
    | EnvironmentLoaded EnvironmentScopedPage (Result Http.Error Environment)


type ProjectScopedPage
    = ScopedNewEnvironment
    | ScopedEditProject


type EnvironmentScopedPage
    = ScopedEditEnvironment


type Content
    = EditProject EditProject.Model
    | NewProject NewProject.Model
    | NewEnvironment NewEnvironment.Model
    | EditEnvironment EditEnvironment.Model
    | ProjectsList ProjectsList.Model
    | Settings Settings.Model
    | Loading


type Page
    = NotFound
    | App Topbar.Model Content


type alias Model =
    { page : Page, apiToken : String }


wrapTopbar : Model -> Content -> Cmd Msg -> ( Topbar.Model, List (Cmd Topbar.Msg) ) -> ( Model, Cmd Msg )
wrapTopbar model content cmds ( subModel, subCmds ) =
    let
        cmd =
            Util.processCmds subCmds
                |> Cmd.map TopbarMsg
    in
    ( { model | page = App subModel content }, Cmd.batch [ cmd, cmds ] )


withTopbar : Model -> Content -> Cmd Msg -> ( Model, Cmd Msg )
withTopbar model content cmd =
    case model.page of
        App topbar _ ->
            ( { model | page = App topbar content }, cmd )

        NotFound ->
            Topbar.init
                |> wrapTopbar model content cmd


wrapPage : (model -> Content) -> (msg -> Msg) -> Model -> ( model, List (Cmd msg) ) -> ( Model, Cmd Msg )
wrapPage toContent toMsg model ( subModel, subCmds ) =
    let
        cmd =
            Util.processCmds subCmds
                |> Cmd.map toMsg

        content =
            toContent subModel
    in
    withTopbar model content cmd


scopedByProject : Model -> String -> ProjectScopedPage -> ( Model, Cmd Msg )
scopedByProject model projectId page =
    let
        cmd =
            getProject model.apiToken projectId (ProjectLoaded page)
    in
    withTopbar model Loading cmd


scopedByEnvironment : Model -> String -> EnvironmentScopedPage -> ( Model, Cmd Msg )
scopedByEnvironment model environmentId page =
    let
        cmd =
            getEnvironment model.apiToken environmentId (EnvironmentLoaded page)
    in
    withTopbar model Loading cmd


setPage : Model -> Navigation.Location -> ( Model, Cmd Msg )
setPage model location =
    case Route.readProtectedRoute location of
        Nothing ->
            ( { model | page = NotFound }, Cmd.none )

        Just Route.ProjectsList ->
            ProjectsList.init model.apiToken
                |> wrapPage ProjectsList ProjectsListMsg model

        Just Route.NewProject ->
            NewProject.init model.apiToken
                |> wrapPage NewProject NewProjectMsg model

        Just (Route.EditProject projectId) ->
            scopedByProject model projectId ScopedEditProject

        Just (Route.NewEnvironment projectId) ->
            scopedByProject model projectId ScopedNewEnvironment

        Just (Route.EditEnvironment environmentId) ->
            scopedByEnvironment model environmentId ScopedEditEnvironment

        Just (Route.Settings code) ->
            Settings.init model.apiToken code
                |> wrapPage Settings SettingsMsg model


init : String -> Navigation.Location -> ( Model, Cmd Msg )
init apiToken =
    setPage (Model NotFound apiToken)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlChanged location ->
            setPage model location

        EditProjectMsg subMsg ->
            case model.page of
                App _ (EditProject subModel) ->
                    EditProject.update subMsg subModel
                        |> wrapPage EditProject EditProjectMsg model

                _ ->
                    ( model, Cmd.none )

        EditEnvironmentMsg subMsg ->
            case model.page of
                App _ (EditEnvironment subModel) ->
                    EditEnvironment.update subMsg subModel
                        |> wrapPage EditEnvironment EditEnvironmentMsg model

                _ ->
                    ( model, Cmd.none )

        EnvironmentLoaded page result ->
            case result of
                Err _ ->
                    ( { model | page = NotFound }, Cmd.none )

                Ok environment ->
                    case page of
                        ScopedEditEnvironment ->
                            EditEnvironment.init model.apiToken environment
                                |> wrapPage EditEnvironment EditEnvironmentMsg model

        ProjectsListMsg subMsg ->
            case model.page of
                App _ (ProjectsList subModel) ->
                    ProjectsList.update subMsg subModel
                        |> wrapPage ProjectsList ProjectsListMsg model

                _ ->
                    ( model, Cmd.none )

        NewProjectMsg subMsg ->
            case model.page of
                App _ (NewProject subModel) ->
                    NewProject.update subMsg subModel
                        |> wrapPage NewProject NewProjectMsg model

                _ ->
                    ( model, Cmd.none )

        NewEnvironmentMsg subMsg ->
            case model.page of
                App _ (NewEnvironment subModel) ->
                    NewEnvironment.update subMsg subModel
                        |> wrapPage NewEnvironment NewEnvironmentMsg model

                _ ->
                    ( model, Cmd.none )

        SettingsMsg subMsg ->
            case model.page of
                App _ (Settings subModel) ->
                    Settings.update subMsg subModel
                        |> wrapPage Settings SettingsMsg model

                _ ->
                    ( model, Cmd.none )

        TopbarMsg subMsg ->
            case model.page of
                App subModel content ->
                    Topbar.update subMsg subModel
                        |> wrapTopbar model content Cmd.none

                NotFound ->
                    ( model, Cmd.none )

        ProjectLoaded page result ->
            case result of
                Err _ ->
                    ( { model | page = NotFound }, Cmd.none )

                Ok project ->
                    case page of
                        ScopedNewEnvironment ->
                            NewEnvironment.init model.apiToken project
                                |> wrapPage NewEnvironment NewEnvironmentMsg model

                        ScopedEditProject ->
                            EditProject.init model.apiToken project
                                |> wrapPage EditProject EditProjectMsg model


inLayout : Topbar.Model -> Html.Html Msg -> Html.Html Msg
inLayout tobarModel page =
    Html.div []
        [ tobarModel |> Topbar.view |> Html.map TopbarMsg
        , page
        ]


contentView : Content -> Html.Html Msg
contentView content =
    case content of
        EditEnvironment subModel ->
            EditEnvironment.view subModel
                |> Html.map EditEnvironmentMsg

        EditProject subModel ->
            EditProject.view subModel
                |> Html.map EditProjectMsg

        ProjectsList subModel ->
            ProjectsList.view subModel
                |> Html.map ProjectsListMsg

        NewProject subModel ->
            NewProject.view subModel
                |> Html.map NewProjectMsg

        NewEnvironment subModel ->
            NewEnvironment.view subModel
                |> Html.map NewEnvironmentMsg

        Settings subModel ->
            Settings.view subModel
                |> Html.map SettingsMsg

        Loading ->
            Html.div [] []


view : Model -> Html.Html Msg
view model =
    case model.page of
        NotFound ->
            NotFound.view

        App tobar content ->
            contentView content
                |> inLayout tobar


main : Program String Model Msg
main =
    Navigation.programWithFlags UrlChanged
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
