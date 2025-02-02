module App.Main exposing
    ( Model
    , Msg(..)
    , init
    , subscriptions
    , update
    , view
    )

import Api
import App.Fragments.Topbar.Main as Topbar
import App.Pages.Environments.Edit.Main as EditEnvironment
import App.Pages.Environments.New.Main as NewEnvironment
import App.Pages.NotFound as NotFound
import App.Pages.Projects.Edit.Main as EditProject
import App.Pages.Projects.List.Main as ProjectsList
import App.Pages.Projects.New.Main as NewProject
import App.Pages.SlackCallback as SlackCallback
import Html
import Route
import Util


type Msg
    = UrlChanged Route.ProtectedRoute
    | ProjectsListMsg ProjectsList.Msg
    | NewProjectMsg NewProject.Msg
    | EditProjectMsg EditProject.Msg
    | NewEnvironmentMsg NewEnvironment.Msg
    | EditEnvironmentMsg EditEnvironment.Msg
    | TopbarMsg Topbar.Msg
    | SlackCallbackMsg SlackCallback.Msg


type Content
    = EditProject EditProject.Model
    | NewProject NewProject.Model
    | NewEnvironment NewEnvironment.Model
    | EditEnvironment EditEnvironment.Model
    | ProjectsList ProjectsList.Model
    | SlackCallback SlackCallback.Model
    | Loading


type Page
    = NotFound
    | App Topbar.Model Content


type alias Model =
    { page : Page
    , apiConfig : Api.ProtectedConfig
    , navigationKey : Route.NavigationKey
    }


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
            Topbar.init model.apiConfig
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


setPage : Model -> Route.ProtectedRoute -> ( Model, Cmd Msg )
setPage model page =
    case page of
        Route.Home ->
            ProjectsList.init model.apiConfig
                |> wrapPage ProjectsList ProjectsListMsg model

        Route.ProjectsList ->
            ProjectsList.init model.apiConfig
                |> wrapPage ProjectsList ProjectsListMsg model

        Route.NewProject step ->
            NewProject.init model.apiConfig model.navigationKey step
                |> wrapPage NewProject NewProjectMsg model

        Route.EditProject projectId ->
            EditProject.init model.apiConfig model.navigationKey projectId
                |> wrapPage EditProject EditProjectMsg model

        Route.NewEnvironment projectId ->
            NewEnvironment.init model.apiConfig model.navigationKey projectId
                |> wrapPage NewEnvironment NewEnvironmentMsg model

        Route.EditEnvironment environmentId ->
            EditEnvironment.init model.apiConfig model.navigationKey environmentId
                |> wrapPage EditEnvironment EditEnvironmentMsg model

        Route.SlackCallback code state ->
            SlackCallback.init model.apiConfig model.navigationKey code state
                |> wrapPage SlackCallback SlackCallbackMsg model


init : Api.ProtectedConfig -> Route.NavigationKey -> Route.ProtectedRoute -> ( Model, Cmd Msg )
init apiConfig navigationKey =
    setPage (Model NotFound apiConfig navigationKey)


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

        SlackCallbackMsg subMsg ->
            case model.page of
                App _ (SlackCallback subModel) ->
                    SlackCallback.update subMsg subModel
                        |> wrapPage SlackCallback SlackCallbackMsg model

                _ ->
                    ( model, Cmd.none )

        TopbarMsg subMsg ->
            case model.page of
                App subModel content ->
                    Topbar.update subMsg subModel
                        |> wrapTopbar model content Cmd.none

                NotFound ->
                    ( model, Cmd.none )


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

        SlackCallback subModel ->
            SlackCallback.view subModel
                |> Html.map SlackCallbackMsg

        Loading ->
            Html.div [] []


view : Model -> Html.Html Msg
view model =
    case model.page of
        NotFound ->
            NotFound.view

        App topbar content ->
            contentView content
                |> inLayout topbar


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.page of
        NotFound ->
            Sub.none

        App topbarModel contentModel ->
            let
                topbarSubscriptions =
                    Topbar.subscriptions topbarModel
                        |> Sub.map TopbarMsg

                contentSubscriptions =
                    case contentModel of
                        EditProject subModel ->
                            EditProject.subscriptions subModel
                                |> Sub.map EditProjectMsg

                        SlackCallback subModel ->
                            SlackCallback.subscriptions subModel
                                |> Sub.map SlackCallbackMsg

                        _ ->
                            Sub.none
            in
            Sub.batch [ topbarSubscriptions, contentSubscriptions ]
