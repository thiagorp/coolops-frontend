module Public.Main exposing
    ( Model
    , Msg(..)
    , init
    , subscriptions
    , update
    , view
    )

import Html
import Public.Pages.DeploymentLogs.Main as DeploymentLogs
import Public.Pages.SlackCallback as SlackCallback
import Route


type Msg
    = UrlChanged Route.PublicRoute
    | DeploymentLogsMsg DeploymentLogs.Msg
    | SlackCallbackMsg SlackCallback.Msg


type Page
    = Transitioning
    | DeploymentLogs DeploymentLogs.Model
    | SlackCallback SlackCallback.Model


type alias Model =
    { page : Page
    , baseUrl : String
    , navigationKey : Route.NavigationKey
    }


wrapPage : (model -> Page) -> (msg -> Msg) -> Model -> ( model, List (Cmd msg) ) -> ( Model, Cmd Msg )
wrapPage toPage toMsg model ( subModel, subCmds ) =
    let
        subCmd =
            case subCmds of
                [] ->
                    Cmd.none

                cmds ->
                    Cmd.batch subCmds

        cmd =
            Cmd.map toMsg subCmd

        page =
            toPage subModel
    in
    ( { model | page = page }, cmd )


setPage : Model -> Route.PublicRoute -> ( Model, Cmd Msg )
setPage model route =
    case route of
        Route.DeploymentLogs id ->
            DeploymentLogs.init model.baseUrl id
                |> wrapPage DeploymentLogs DeploymentLogsMsg model

        Route.SlackCallback code state ->
            SlackCallback.init model.navigationKey code state
                |> wrapPage SlackCallback SlackCallbackMsg model


init : String -> Route.NavigationKey -> Route.PublicRoute -> ( Model, Cmd Msg )
init baseUrl navigationKey =
    setPage (Model Transitioning baseUrl navigationKey)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlChanged page ->
            setPage model page

        DeploymentLogsMsg subMsg ->
            case model.page of
                DeploymentLogs subModel ->
                    DeploymentLogs.update subMsg subModel
                        |> wrapPage DeploymentLogs DeploymentLogsMsg model

                _ ->
                    ( model, Cmd.none )

        SlackCallbackMsg subMsg ->
            case model.page of
                SlackCallback subModel ->
                    SlackCallback.update subMsg subModel
                        |> wrapPage SlackCallback SlackCallbackMsg model

                _ ->
                    ( model, Cmd.none )


view : Model -> Html.Html Msg
view model =
    case model.page of
        Transitioning ->
            Html.div [] []

        DeploymentLogs subModel ->
            DeploymentLogs.view subModel
                |> Html.map DeploymentLogsMsg

        SlackCallback subModel ->
            SlackCallback.view subModel
                |> Html.map SlackCallbackMsg


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.page of
        DeploymentLogs subModel ->
            DeploymentLogs.subscriptions subModel
                |> Sub.map DeploymentLogsMsg

        SlackCallback subModel ->
            SlackCallback.subscriptions subModel
                |> Sub.map SlackCallbackMsg

        Transitioning ->
            Sub.none
